module Bcc.Wallet.API.V1.Handlers.Info (handlers) where

import           Universum

import           Servant

import           Bcc.Wallet.API.Response (APIResponse, single)
import qualified Bcc.Wallet.API.V1.Info as Info
import           Bcc.Wallet.API.V1.Types (ForceNtpCheck, NodeInfo)
import           Bcc.Wallet.WalletLayer (ActiveWalletLayer)
import qualified Bcc.Wallet.WalletLayer as WalletLayer

handlers :: ActiveWalletLayer IO -> ServerT Info.API Handler
handlers = getNodeInfo

getNodeInfo
    :: ActiveWalletLayer IO
    -> ForceNtpCheck
    -> Handler (APIResponse NodeInfo)
getNodeInfo w forceNtp =
    liftIO $ single <$> WalletLayer.getNodeInfo w forceNtp
