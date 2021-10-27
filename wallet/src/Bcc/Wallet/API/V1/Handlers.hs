module Bcc.Wallet.API.V1.Handlers (handlers) where

import           Servant
import           Universum

import qualified Bcc.Wallet.API.V1 as V1
import qualified Bcc.Wallet.API.V1.Handlers.Accounts as Accounts
import qualified Bcc.Wallet.API.V1.Handlers.Addresses as Addresses
import qualified Bcc.Wallet.API.V1.Handlers.Info as Info
import qualified Bcc.Wallet.API.V1.Handlers.Settings as Settings
import qualified Bcc.Wallet.API.V1.Handlers.Transactions as Transactions
import qualified Bcc.Wallet.API.V1.Handlers.Wallets as Wallets

import           Bcc.Wallet.WalletLayer (ActiveWalletLayer,
                     walletPassiveLayer)


handlers :: ActiveWalletLayer IO -> Server V1.API
handlers w =  Addresses.handlers    passiveWallet
         :<|> Wallets.handlers      passiveWallet
         :<|> Accounts.handlers     passiveWallet
         :<|> Transactions.handlers w
         :<|> Settings.handlers     passiveWallet
         :<|> Info.handlers         w
  where
    passiveWallet = walletPassiveLayer w
