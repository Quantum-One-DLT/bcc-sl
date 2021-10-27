module Bcc.Wallet.API.V1.Handlers.Settings (handlers) where

import           Universum

import           Servant

import           Bcc.Wallet.API.Response (APIResponse, single)
import           Bcc.Wallet.API.V1.Types (NodeSettings)
import           Bcc.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Bcc.Wallet.WalletLayer as WalletLayer

import qualified Pos.Node.API as Node

handlers :: PassiveWalletLayer IO -> ServerT Node.SettingsAPI Handler
handlers = getNodeSettings

-- | Retrieve the static settings for this node
getNodeSettings :: PassiveWalletLayer IO
                -> Handler (APIResponse NodeSettings)
getNodeSettings w = liftIO $ single <$> WalletLayer.getNodeSettings w
