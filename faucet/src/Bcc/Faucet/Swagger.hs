{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module Bcc.Faucet.Swagger
    ( FaucetDoc
    , swaggerServer
    , faucetDoc
    ) where

import           Control.Lens ((?~))
import           Data.Proxy
import           Data.Swagger
import qualified Data.Text as T
import           NeatInterpolation
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI (SwaggerSchemaUI)
import           Universum

import           Bcc.Wallet.API.V1.Swagger
import           Pos.Util.CompileInfo (CompileTimeInfo (..), HasCompileInfo,
                     compileInfo)

import           Bcc.Faucet.Endpoints

--------------------------------------------------------------------------------
-- | Swagger UI type
type FaucetDoc = SwaggerSchemaUI "docs" "swagger.json"

faucetDoc :: Proxy FaucetDoc
faucetDoc = Proxy

--------------------------------------------------------------------------------
-- | Snippet for current bcc version
bccVersion :: T.Text
bccVersion = "bcc-sl:0"

-- | Header documentation
faucetMD :: CompileTimeInfo -> T.Text
faucetMD CompileTimeInfo{..} = [text|
This is the faucet api documentation

The faucet is a component of the test net that allows users to request BCC from
a wallet to their own address for testing.

Software Version   | Git Revision
-------------------|-------------------
$bccVersion           | $ctiGitRevision

 |]

--------------------------------------------------------------------------------
-- | Constructor for the faucet's swagger
mkSwagger :: HasSwagger a
    => CompileTimeInfo
    -> Proxy a
    -> Swagger
mkSwagger ci walletAPI = toSwagger walletAPI
  & info.title   .~ "Bcc Faucet API"
  & info.version .~ bccVersion
  & host ?~ "127.0.0.1:8090"
  & info.description ?~ (faucetMD ci)
  & info.license ?~ ("MIT" & url ?~ URL "https://raw.githubusercontent.com/The-Blockchain-Company/bcc-sl/develop/lib/LICENSE")

--------------------------------------------------------------------------------
-- | Server for the swagger UI
swaggerServer :: (HasCompileInfo) => Server FaucetDoc
swaggerServer = swaggerSchemaUIServer (mkSwagger compileInfo faucetServerAPI)
