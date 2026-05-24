{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Bornet.Api.WholeServer (StaticServeConf (..), makeApplication) where

import Data.Aeson
import Data.Proxy
import GHC.Generics (Generic)
import Bornet.Api
import Network.Wai.Application qualified
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles

data StaticServeConf = StaticServeConf
  { staticAssetsFolder :: FilePath
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

makeApplication :: Server Api -> StaticServeConf -> Network.Wai.Application.Application
makeApplication server StaticServeConf {staticAssetsFolder} = Network.Wai.Application.MakeApplication {Network.Wai.Application.application}
  where
    staticAssetsServer = serveDirectoryWebApp staticAssetsFolder
    application :: Application
    application = serve (Proxy @(Api :<|> "static" :> Raw)) do server :<|> staticAssetsServer
