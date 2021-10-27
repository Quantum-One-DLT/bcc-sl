module Bcc.Wallet.Server
    ( walletServer
    , walletDocServer
    ) where

import           Universum

import           Servant

import           Pos.Chain.Update (HasUpdateConfiguration, curSoftwareVersion,
                     updateConfiguration)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo)

import           Bcc.Wallet.API
import qualified Bcc.Wallet.API.Internal.Handlers as Internal
import qualified Bcc.Wallet.API.V1.Handlers as V1
import           Bcc.Wallet.API.V1.Swagger (swaggerSchemaUIServer)
import qualified Bcc.Wallet.API.V1.Swagger as Swagger
import           Bcc.Wallet.Server.CLI (RunMode (..))
import           Bcc.Wallet.WalletLayer (ActiveWalletLayer (..))

-- | Serve the REST interface to the wallet
--
-- NOTE: Unlike the legacy server, the handlers will not run in a special
-- Bcc monad because they just interfact with the Wallet object.
walletServer :: ActiveWalletLayer IO
             -> RunMode
             -> Server WalletAPI
walletServer w _ =
    v1Handler
    :<|> internalHandler
  where
    v1Handler       = V1.handlers w
    internalHandler = Internal.handlers (walletPassiveLayer w)

walletDocServer :: (HasCompileInfo, HasUpdateConfiguration) => Server WalletDoc
walletDocServer =
    v1DocHandler
  where
    infos        = (compileInfo, curSoftwareVersion updateConfiguration)
    v1DocHandler = swaggerSchemaUIServer
        (Swagger.api infos walletDocAPI Swagger.highLevelDescription)
