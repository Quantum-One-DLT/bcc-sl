{-# LANGUAGE TypeApplications #-}
module Test.Spec.Addresses (spec, withFixture, Fixture(..)) where

import           Universum

import           Control.Monad.Except (runExceptT)
import           Data.Acid (update)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Formatting (build, sformat)
import           Servant.Server

import           Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, choose, elements, withMaxSuccess,
                     (===))
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)

import           Pos.Core (Address, addrRoot)
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, ProtocolMagic, emptyPassphrase,
                     firstHardened, safeDeterministicKeyGen)

import           Bcc.Wallet.API.Request (RequestParams (..))
import           Bcc.Wallet.API.Request.Pagination (Page (..),
                     PaginationParams (..), PerPage (..))
import           Bcc.Wallet.API.Response (APIResponse (..), SliceOf (..))
import           Bcc.Wallet.API.V1.Handlers.Addresses as Handlers
import qualified Bcc.Wallet.API.V1.Types as V1
import qualified Bcc.Wallet.Kernel.Addresses as Kernel
import           Bcc.Wallet.Kernel.DB.AcidState
import           Bcc.Wallet.Kernel.DB.HdWallet (AssuranceLevel (..),
                     HasSpendingPassword (..), HdAccountId (..),
                     HdAccountIx (..), HdRootId (..), WalletName (..),
                     eskToHdRootId)
import           Bcc.Wallet.Kernel.DB.HdWallet.Create (initHdRoot)
import           Bcc.Wallet.Kernel.DB.HdWallet.Derivation
                     (HardeningMode (..), deriveIndex)
import           Bcc.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import qualified Bcc.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Bcc.Wallet.Kernel.Internal (PassiveWallet, wallets)
import qualified Bcc.Wallet.Kernel.Keystore as Keystore
import qualified Bcc.Wallet.Kernel.Read as Kernel
import           Bcc.Wallet.Kernel.Types (AccountId (..), WalletId (..))
import qualified Bcc.Wallet.Kernel.Wallets as Kernel
import           Bcc.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Bcc.Wallet.WalletLayer as WalletLayer
import qualified Bcc.Wallet.WalletLayer.Kernel.Accounts as Accounts
import qualified Bcc.Wallet.WalletLayer.Kernel.Addresses as Addresses
import qualified Bcc.Wallet.WalletLayer.Kernel.Conv as Kernel.Conv
import qualified Bcc.Wallet.WalletLayer.Kernel.Wallets as Wallets

import qualified Test.Spec.Fixture as Fixture
import qualified Test.Spec.Wallets as Wallets

import           Util.Buildable (ShowThroughBuild (..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data Fixture = Fixture {
      fixtureHdRootId  :: HdRootId
    , fixtureESK       :: EncryptedSecretKey
    , fixtureAccountId :: AccountId
    , fixturePw        :: PassiveWallet
    }

data AddressFixture = AddressFixture {
    addressFixtureAddress :: V1.WalletAddress
    } deriving (Eq, Ord)

-- | Prepare some fixtures using the 'PropertyM' context to prepare the data,
-- and execute the 'acid-state' update once the 'PassiveWallet' gets into
-- scope (after the bracket initialisation).
prepareFixtures :: NetworkMagic -> Fixture.GenPassiveWalletFixture Fixture
prepareFixtures nm = do
    let (_, esk) = safeDeterministicKeyGen (B.pack $ replicate 32 0x42) mempty
    let newRootId = eskToHdRootId nm esk
    newRoot <- initHdRoot <$> pure newRootId
                          <*> pure (WalletName "A wallet")
                          <*> pure NoSpendingPassword
                          <*> pure AssuranceLevelNormal
                          <*> (InDb <$> pick arbitrary)
    newAccountId <- HdAccountId newRootId <$> deriveIndex (pick . choose) HdAccountIx HardDerivation
    let accounts = M.singleton newAccountId mempty
        hdAccountId = Kernel.defaultHdAccountId newRootId
        hdAddress   = Kernel.defaultHdAddress nm esk emptyPassphrase newRootId

    return $ \pw -> do
        void $ liftIO $ update (pw ^. wallets) (CreateHdWallet newRoot hdAccountId hdAddress accounts)
        return $ Fixture {
                           fixtureHdRootId = newRootId
                         , fixtureAccountId = AccountIdHdRnd newAccountId
                         , fixtureESK = esk
                         , fixturePw  = pw
                         }

prepareAddressFixture
  :: Int  -- ^ Number of 'AddressFixture's to create.
  -> Fixture.GenPassiveWalletFixture [AddressFixture]
prepareAddressFixture n = do
    spendingPassword <- Fixture.genSpendingPassword
    newWalletRq <- WalletLayer.CreateWallet <$> Wallets.genNewWalletRq spendingPassword
    return $ \pw -> do
        Right v1Wallet <- Wallets.createWallet pw newWalletRq
        -- Create new accounts under the first, automatically-generated
        -- account for this wallet, placed at 'firstHardened'.
        let newAddressRq =
                V1.NewAddress spendingPassword
                              (V1.unsafeMkAccountIndex firstHardened)
                              (V1.walId v1Wallet)

        -- We create one address less of which is requested by the caller, as
        -- by default each fresh account gets a new address.
        res <- replicateM (max 0 (n -1)) (Addresses.createAddress pw newAddressRq)
        case sequence res of
            Left e     -> error (show e)
            Right _addrs -> do
                db' <- Kernel.getWalletSnapshot pw
                -- Low & behold here lies an hack: in order to produce the data
                -- in the same order in which it will be consumed by the
                -- pagination tests, we need to call 'listAddresses', which
                -- obviously doesn't list _just_ the addresses for a single
                -- account, but it works as our fixture creates only one.
                let pp = PaginationParams (Page 1) (PerPage 100)
                let SliceOf{..} = Addresses.getAddresses (RequestParams pp) db'
                return . map AddressFixture $ paginatedSlice

newtype DesiredNewAccounts  = DesiredNewAccs Int
newtype DesiredNewAddresses = DesiredNewAddrs Int

prepareAddressesFixture
    :: DesiredNewAccounts   -- ^ Number of Accounts to create.
    -> DesiredNewAddresses  -- ^ Number of Address per account to create.
    -> Fixture.GenPassiveWalletFixture (M.Map V1.AccountIndex [V1.WalletAddress], Int)
prepareAddressesFixture (DesiredNewAccs acn) (DesiredNewAddrs adn) = do
    spendingPassword <- Fixture.genSpendingPassword
    newWalletRq <- WalletLayer.CreateWallet <$> Wallets.genNewWalletRq spendingPassword
    return $ \pw -> do
        let newAcc (n :: Int) = (V1.NewAccount spendingPassword ("My Account " <> show n))
        Right v1Wallet <- Wallets.createWallet pw newWalletRq
        forM_ [1..acn] $ \n ->
            Accounts.createAccount pw (V1.walId v1Wallet) (newAcc n)
        -- Get all the available accounts
        db <- Kernel.getWalletSnapshot pw
        let Right accs = Accounts.getAccounts (V1.walId v1Wallet) db
        let accounts = IxSet.toList accs
        length accounts `shouldBe` (acn + 1)
        let insertAddresses :: V1.Account -> IO (V1.AccountIndex, [V1.WalletAddress])
            insertAddresses acc = do
                let accId = V1.accIndex acc
                let newAddressRq = V1.NewAddress spendingPassword accId (V1.walId v1Wallet)
                res <- replicateM adn (Addresses.createAddress pw newAddressRq)
                case sequence res of
                    Left e     -> error (show e)
                    Right addr -> return (accId, addr)
        res <- mapM insertAddresses accounts
        -- This takes into account that each wallet creates by default one account, which has
        -- one address.
        return $ (M.fromList res, (acn + 1)*(adn + 1) - acn)

withFixture :: ProtocolMagic
            -> (  Keystore.Keystore
               -> PassiveWalletLayer IO
               -> PassiveWallet
               -> Fixture
               -> IO a
               )
            -> PropertyM IO a
withFixture pm = Fixture.withPassiveWalletFixture pm (prepareFixtures nm)
  where
    nm = makeNetworkMagic pm

withAddressFixtures
  :: ProtocolMagic
  -> Int -- Number of fixture addresses to create
  -> (  Keystore.Keystore
     -> PassiveWalletLayer IO
     -> PassiveWallet
     -> [AddressFixture]
     -> IO a
     )
  -> PropertyM IO a
withAddressFixtures pm n =
  Fixture.withPassiveWalletFixture pm $ do
      prepareAddressFixture n

withAddressesFixtures
    :: ProtocolMagic
    -> DesiredNewAccounts
    -> DesiredNewAddresses
    -> (  Keystore.Keystore
    -> PassiveWalletLayer IO
    -> PassiveWallet
    -> (M.Map V1.AccountIndex [V1.WalletAddress], Int)
    -> IO a
    )
    -> PropertyM IO a
withAddressesFixtures pm n m =
    Fixture.withPassiveWalletFixture pm $ do
        prepareAddressesFixture n m

spec :: Spec
spec = describe "Addresses" $ do
    describe "CreateAddress" $ do
        describe "Address creation (wallet layer)" $ do
            prop "works as expected in the happy path scenario" $ withMaxSuccess 5 $
                monadicIO $ do
                    pm <- pick arbitrary
                    withFixture pm $ \keystore layer _ Fixture{..} -> do
                        Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                        let (HdRootId hdRoot) = fixtureHdRootId
                            (AccountIdHdRnd myAccountId) = fixtureAccountId
                            wId = sformat build (view fromDb hdRoot)
                            accIdx = Kernel.Conv.toAccountId myAccountId
                        res <- WalletLayer.createAddress layer (V1.NewAddress Nothing accIdx (V1.WalletId wId))
                        (bimap STB STB res) `shouldSatisfy` isRight

        describe "Address creation (kernel)" $ do
            prop "works as expected in the happy path scenario" $ withMaxSuccess 5 $
                monadicIO $ do
                    pm <- pick arbitrary
                    withFixture pm $ \keystore _ _ Fixture{..} -> do
                        Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                        res <- Kernel.createAddress mempty fixtureAccountId fixturePw
                        (bimap STB STB res) `shouldSatisfy` isRight

            prop "fails if the account has no associated key in the keystore" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withFixture pm $ \_ _ _ Fixture{..} -> do
                        res <- Kernel.createAddress mempty fixtureAccountId fixturePw
                        case res of
                            (Left (Kernel.CreateAddressKeystoreNotFound acc)) | acc == fixtureAccountId -> return ()
                            x -> fail (show (bimap STB STB x))

            prop "fails if the parent account doesn't exist" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withFixture pm $ \keystore _ _ Fixture{..} -> do
                        Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                        let (AccountIdHdRnd hdAccountId) = fixtureAccountId
                        void $ update (fixturePw ^. wallets) (DeleteHdAccount hdAccountId)
                        res <- Kernel.createAddress mempty fixtureAccountId fixturePw
                        case res of
                            Left (Kernel.CreateAddressUnknownHdAccount _) -> return ()
                            x -> fail (show (bimap STB STB x))

        describe "Address creation (Servant)" $ do
            prop "works as expected in the happy path scenario" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withFixture pm $ \keystore layer _ Fixture{..} -> do
                        Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                        let (HdRootId hdRoot) = fixtureHdRootId
                            (AccountIdHdRnd myAccountId) = fixtureAccountId
                            wId = sformat build (view fromDb hdRoot)
                            accIdx = Kernel.Conv.toAccountId myAccountId
                            req = V1.NewAddress Nothing accIdx (V1.WalletId wId)
                        res <- runExceptT . runHandler' $ Handlers.newAddress layer req
                        (bimap identity STB res) `shouldSatisfy` isRight

        describe "Address creation (wallet layer & kernel consistency)" $ do
            prop "layer & kernel agrees on the result" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    res1 <- withFixture pm $ \keystore _ _ Fixture{..} -> do
                        Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                        Kernel.createAddress mempty fixtureAccountId fixturePw
                    res2 <- withFixture pm $ \keystore layer _ Fixture{..} -> do
                        Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                        let (HdRootId hdRoot) = fixtureHdRootId
                            (AccountIdHdRnd myAccountId) = fixtureAccountId
                            wId = sformat build (view fromDb hdRoot)
                            accIdx = Kernel.Conv.toAccountId myAccountId
                        WalletLayer.createAddress layer (V1.NewAddress Nothing accIdx (V1.WalletId wId))
                    case res2 of
                         Left (WalletLayer.CreateAddressError err) ->
                             return $ (bimap STB STB res1) `shouldBe` (bimap STB STB (Left err))
                         Left (WalletLayer.CreateAddressAddressDecodingFailed _) ->
                             fail "Layer & Kernel mismatch: impossible error, CreateAddressAddressDecodingFailed"
                         Right _ -> do
                             -- If we get and 'Address', let's check that this is the case also for
                             -- the kernel run. Unfortunately we cannot compare the two addresses for equality
                             -- because the random index will be generated with a seed which changes every time
                             -- as we uses random, IO-based generation deep down the guts.
                             return $ (bimap STB STB res1) `shouldSatisfy` isRight

        describe "Address listing (Servant)" $ do
            prop "0 addresses, page 0, per page 0" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressFixtures pm 0 $ \_ layer _ _ -> do
                        let pp = PaginationParams (Page 0) (PerPage 0)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | null (wrData wr) -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "1 addresses, page 0, per page 0" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressFixtures pm 1 $ \_ layer _ _ -> do
                        let pp = PaginationParams (Page 0) (PerPage 0)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | null (wrData wr) -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "3 addresses, page 0, per page 0" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressFixtures pm 3 $ \_ layer _ _ -> do
                        let pp = PaginationParams (Page 0) (PerPage 0)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | null (wrData wr) -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "3 addresses, page 1, per page 0" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressFixtures pm 3 $ \_ layer _ _ -> do
                        let pp = PaginationParams (Page 1) (PerPage 0)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | null (wrData wr) -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "3 addresses, page 1, per page 1" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressFixtures pm 3 $ \_ layer _ [wa0, _, _] -> do
                        let pp = PaginationParams (Page 1) (PerPage 1)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                            Right wr ->
                                wrData wr `shouldBe` [addressFixtureAddress wa0]
                            _ -> fail ("Got " ++ show res)

            prop "3 addresses, page 1, per page 2" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressFixtures pm 3 $ \_ layer _ [wa0, wa1, _wa2] -> do
                        let pp = PaginationParams (Page 1) (PerPage 2)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | [wa0', wa1'] <- wrData wr
                                    , wa0' == addressFixtureAddress wa0
                                    , wa1' == addressFixtureAddress wa1
                                    -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "3 addresses, page 1, per page 3" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressFixtures pm 3 $ \_ layer _ [wa0, wa1, wa2] -> do
                        let pp = PaginationParams (Page 1) (PerPage 3)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | [wa0', wa1', wa2'] <- wrData wr
                                    , wa0' == addressFixtureAddress wa0
                                    , wa1' == addressFixtureAddress wa1
                                    , wa2' == addressFixtureAddress wa2
                                    -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "3 addresses, page 2, per page 2" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressFixtures pm 3 $ \_ layer _ [_wa0, _wa1, wa2] -> do
                        let pp = PaginationParams (Page 2) (PerPage 2)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | [wa2'] <- wrData wr
                                    , wa2' == addressFixtureAddress wa2
                                    -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "4 addresses, page 2, per page 2" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressFixtures pm 4 $ \_ layer _ [_wa0, _wa1, wa2, wa3] -> do
                        let pp = PaginationParams (Page 2) (PerPage 2)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | [wa2', wa3'] <- wrData wr
                                    , wa2' == addressFixtureAddress wa2
                                    , wa3' == addressFixtureAddress wa3
                                    -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "arbitrary number of addresses, pages and per page" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    (rNumOfAddresses :: Int) <- pick $ elements [0..15]
                    (rNumOfPages :: Int) <- pick $ elements [0..15]
                    (rNumPerPage :: Int) <- pick $ elements [0..15]
                    pm <- pick arbitrary
                    withAddressFixtures pm rNumOfAddresses $ \_ layer _ fixtureAddresses -> do
                        let (!>) = drop . (subtract 1)
                        let (<!) = flip take
                        let slice numOfPage numPerPage xs
                                | numOfPage*numPerPage == 0 = []
                                | otherwise = ((numOfPage-1)*numPerPage+1) !> xs <! numPerPage
                        let pp = PaginationParams (Page rNumOfPages) (PerPage rNumPerPage)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        let toBeCheckedAddresses = wrData <$> res
                        let correctAddresses = pure $ map addressFixtureAddress $
                                               slice rNumOfPages rNumPerPage fixtureAddresses
                        pure (toBeCheckedAddresses === correctAddresses)

        describe "Address listing with multiple Accounts (Servant)" $ do
            let rootId = addrRoot . V1.unV1 . V1.addrId
            prop "page 0, per page 0" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressesFixtures pm (DesiredNewAccs 4) (DesiredNewAddrs 4) $ \_ layer _ _ -> do
                        let pp = PaginationParams (Page 0) (PerPage 0)
                        res <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp)
                        case res of
                            Right wr | null (wrData wr) -> pure ()
                            _ -> fail ("Got " ++ show res)

            prop "it yields the correct number of results" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressesFixtures pm (DesiredNewAccs 3) (DesiredNewAddrs 4) $ \_ layer _ (_, total) -> do
                        let pp = PaginationParams (Page 1) (PerPage 40)
                        res <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp)
                        case res of
                            Right wr -> do
                                length (wrData wr) `shouldBe` min 40 total
                            _        -> fail ("Got " ++ show res)

            prop "is deterministic" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressesFixtures pm (DesiredNewAccs 3) (DesiredNewAddrs 8) $ \_ layer _ (_, expectedTotal) -> do
                        let ppSplit = quot expectedTotal 3 + 1
                            mkRequest mypp = Handlers.listAddresses layer (RequestParams mypp)
                        forM_ [(1,40), (1, ppSplit), (2, ppSplit), (3, ppSplit)] $ \(page, perPage) -> do
                            let pp = PaginationParams (Page page) (PerPage perPage)
                            Right (r1, r2) <- runExceptT $ runHandler' $ do
                                              (,) <$> mkRequest pp <*> mkRequest pp
                            r1 `shouldBe` r2

            prop "yields the correct set of resutls" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressesFixtures pm (DesiredNewAccs 4) (DesiredNewAddrs 8) $ \_ layer _ (_, expectedTotal) -> do
                        let ppSplit = quot expectedTotal 3 + 1
                            pp = PaginationParams (Page 1) (PerPage 50)
                            pp1 = PaginationParams (Page 1) (PerPage ppSplit)
                            pp2 = PaginationParams (Page 2) (PerPage ppSplit)
                            pp3 = PaginationParams (Page 3) (PerPage ppSplit)
                        res <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp)
                        res1 <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp1)
                        res2 <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp2)
                        res3 <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp3)
                        case (res, res1, res2, res3) of
                            (Right wr, Right wr1, Right wr2, Right wr3) -> do
                                let con = wrData wr1 <> wrData wr2 <> wrData wr3
                                length (wrData wr) `shouldBe` expectedTotal
                                length con `shouldBe` expectedTotal
                                S.fromList con `shouldBe` S.fromList (wrData wr)
                                (rootId <$> con) `shouldBe` (rootId <$> wrData wr)
                            _        -> fail ("Got " ++ show res)

            prop "yields the correct ordered resutls when there is one account" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressesFixtures pm (DesiredNewAccs 0) (DesiredNewAddrs 15) $ \_ layer _ (_, expectedTotal) -> do
                        let ppSplit = quot expectedTotal 3 + 1
                            pp = PaginationParams (Page 1) (PerPage 50)
                            pp1 = PaginationParams (Page 1) (PerPage ppSplit)
                            pp2 = PaginationParams (Page 2) (PerPage ppSplit)
                            pp3 = PaginationParams (Page 3) (PerPage ppSplit)
                        res <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp)
                        res1 <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp1)
                        res2 <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp2)
                        res3 <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp3)
                        case (res, res1, res2, res3) of
                            (Right wr, Right wr1, Right wr2, Right wr3) -> do
                                let con = wrData wr1 <> wrData wr2 <> wrData wr3
                                length (wrData wr) `shouldBe` expectedTotal
                                length con `shouldBe` expectedTotal
                                S.fromList con `shouldBe` S.fromList (wrData wr)
                                (rootId <$> con) `shouldBe` (rootId <$> wrData wr)
                            _        -> fail ("Got " ++ show res)


            prop "yields the correct ordered resutls" $ withMaxSuccess 5 $ do
                monadicIO $ do
                  pm <- pick arbitrary
                  forM_ [(DesiredNewAccs 4,DesiredNewAddrs 8),
                         (DesiredNewAccs 6,DesiredNewAddrs 6),
                         (DesiredNewAccs 5,DesiredNewAddrs 7)] $ \(acc,adr) ->
                    withAddressesFixtures pm acc adr $ \_ layer _ (_, expectedTotal) -> do
                        forM_ [2..10] $ \k -> do
                            let indexes = [1..k]
                                pagesParams = map (\i -> PaginationParams (Page i) (PerPage (quot expectedTotal k + 1)))
                                              indexes
                                pp = PaginationParams (Page 1) (PerPage 50)
                            res <- runExceptT $ runHandler' $ do
                                Handlers.listAddresses layer (RequestParams pp)
                            eiResultsArray <- forM pagesParams $ \ppi -> runExceptT $ runHandler' $ do
                                Handlers.listAddresses layer (RequestParams ppi)
                            let resultsArray = sequence eiResultsArray
                            case (res, resultsArray) of
                                (Right wr, Right wrList) -> do
                                    let con = mconcat $ map wrData wrList
                                    length (wrData wr) `shouldBe` expectedTotal
                                    length con `shouldBe` expectedTotal
                                    S.fromList con `shouldBe` S.fromList (wrData wr)
                                    (rootId <$> con)`shouldBe` (rootId <$> wrData wr)
                                _        -> fail ("Got " ++ show res)

    describe "ValidateAddress" $ do
        describe "Address validation (wallet layer)" $ do

            prop "works as expected in the happy path scenario (valid address, ours)" $ withMaxSuccess 5 $
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressFixtures pm 1 $ \_ layer _ [af] -> do
                        res <- WalletLayer.validateAddress layer
                            (sformat build (V1.unV1 $ V1.addrId $ addressFixtureAddress af))
                        bimap STB STB res `shouldSatisfy` isRight

            prop "rejects a malformed address" $ withMaxSuccess 5 $
                monadicIO $ do
                    pm <- pick arbitrary
                    withAddressFixtures pm 1 $ \_ layer _ _ -> do
                        res <- WalletLayer.validateAddress layer "foobar"
                        case res of
                             Left (WalletLayer.ValidateAddressDecodingFailed "foobar") -> return ()
                             Left err -> fail $ "Got different error than expected: " <> show err
                             Right _ -> fail "I was expecting a failure, but it didn't happen."

            prop "returns not used/not change for an address which is not ours" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    (randomAddr :: Address) <- pick arbitrary
                    pm                      <- pick arbitrary
                    let expected :: V1.WalletAddress
                        expected = V1.WalletAddress {
                            addrId            = V1.V1 randomAddr
                          , addrUsed          = False
                          , addrChangeAddress = False
                          , addrOwnership     = V1.V1 V1.AddressAmbiguousOwnership
                          }
                    withAddressFixtures pm 1 $ \_ layer _ _ -> do
                        res <- WalletLayer.validateAddress layer (sformat build randomAddr)
                        bimap STB STB res `shouldBe` bimap STB STB (Right expected)
