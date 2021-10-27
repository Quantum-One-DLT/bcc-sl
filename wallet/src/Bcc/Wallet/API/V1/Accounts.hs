module Bcc.Wallet.API.V1.Accounts where

import           Servant

import           Bcc.Wallet.API.Request
import           Bcc.Wallet.API.Response
import           Bcc.Wallet.API.Types
import           Bcc.Wallet.API.V1.Parameters
import           Bcc.Wallet.API.V1.Types

import qualified Pos.Core as Core


type API
    = Tag "Accounts" 'NoTagDescription :>
    (    "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId
          :> Summary "Deletes an Account."
          :> DeleteNoContent '[ValidJSON] NoContent
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId
          :> Summary "Retrieves a specific Account."
          :> Get '[ValidJSON] (APIResponse Account)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> WalletRequestParams
          :> Summary "Retrieves the full list of Accounts."
          :> Get '[ValidJSON] (APIResponse [Account])
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> Summary "Creates a new Account for the given Wallet."
          :> ReqBody '[ValidJSON] (New Account)
          :> Post '[ValidJSON] (APIResponse Account)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId
          :> Summary "Update an Account for the given Wallet."
          :> ReqBody '[ValidJSON] (Update Account)
          :> Put '[ValidJSON] (APIResponse Account)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId :> "addresses"
          :> Summary "Retrieve only account's addresses."
          :> WalletRequestParams
          :> FilterBy '[V1 Core.Address] WalletAddress
          :> Get '[ValidJSON] (APIResponse AccountAddresses)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId :> "amount"
          :> Summary "Retrieve only account's balance."
          :> Get '[ValidJSON] (APIResponse AccountBalance)
    )
