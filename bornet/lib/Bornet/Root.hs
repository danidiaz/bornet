{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Bornet.Root
  ( cauldron,
    main,
  )
where

import Cauldron
-- import Cauldron.Args
import Cauldron.Managed
-- import Control.Monad.IO.Class

-- import Data.Functor ((<&>))

import Control.Exception (throwIO)
import Control.Monad(join)
import Data.Function ((&))
import Data.Pool.Introspection.Bean (PoolConfig)
import Bornet.Api (BornetLinks, makeLinks)
import Bornet.Api.Server
import Bornet.Api.WholeServer
import Bornet.Repository
import Bornet.Repository.Sqlite qualified
import Sqlite.Bean
import JsonConfig
import JsonConfig.YamlFile qualified
import Log
import Log.Backend.StandardOutput
import Network.Wai.Application
import Network.Wai.Handler.Warp.Runner
import Sqlite (Connection)
import ThreadLocal

cauldron :: Cauldron Managed
cauldron =
  mconcat
    [ let makeJsonConfig = JsonConfig.YamlFile.make $ JsonConfig.YamlFile.loadYamlSettings ["conf.yaml"] [] JsonConfig.YamlFile.useEnv
       in recipe @JsonConfig $ ioEff_ $ pure makeJsonConfig,
      recipe @Logger $ eff_ $ pure $ managed withStdOutLogger,
      recipe @SqlitePoolConfig $ ioEff_ $ wire $ JsonConfig.lookupSection @SqlitePoolConfig "sqlite",
      recipe @PoolConfig $ ioEff_ $ wire $ JsonConfig.lookupSection @PoolConfig "sqlite",
      recipe @SqlitePool $ eff_ $ wire $ \sconf pconf -> managed $ Sqlite.Bean.makeSqlitePool sconf pconf,
      recipe @(ThreadLocal (IO Connection)) $ ioEff_ $ pure makeThreadLocal,
      recipe @(IO Connection) $ val_ $ wire $ join . readThreadLocal @(IO Connection),
      recipe @BornetRepository $ val_ $ wire $ Bornet.Repository.Sqlite.make,
      recipe @BornetLinks $ ioEff_ $ pure makeLinks,
      recipe @BornetServer $
        Recipe
          { bare = val_ $ wire makeBornetServer,
            decos =
              [ val_ $ wire $ Sqlite.Bean.hoistWithLazilyAllocatedConnection Bornet.Api.Server.hoistBornetServer
              ]
          },
      recipe @StaticServeConfig $ ioEff_ $ wire $ JsonConfig.lookupSection @StaticServeConfig "runner",
      recipe @Application $ val_ $ Bornet.Api.WholeServer.makeApplication <$> fmap Bornet.Api.Server.unwrap arg <*> arg,
      recipe @RunnerConfig $ ioEff_ $ wire $ JsonConfig.lookupSection @RunnerConfig "runner",
      recipe @Network.Wai.Handler.Warp.Runner.Settings $ val_ $ wire $ Network.Wai.Handler.Warp.Runner.makeSettings,
      recipe @Runner $
        Recipe
          { bare = val_ $ wire $ Network.Wai.Handler.Warp.Runner.make,
            decos =
              [ val_ $ wire $ \logger (conf :: RunnerConfig) -> Network.Wai.Handler.Warp.Runner.decorate \action -> do
                  logInfo "Server started" conf & runLogT "runner" logger defaultLogLevel
                  action
              ]
          }
    ]

-- <> [
--   recipe @BornetRepository $ ioEff_ $ pure Bornet.Repository.Memory.make
-- ]

main :: IO ()
main = do
  cauldron
    & cook @Runner forbidDepCycles
    & either throwIO \action ->
      with action Network.Wai.Handler.Warp.Runner.run
