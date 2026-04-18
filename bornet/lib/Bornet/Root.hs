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
import Data.Function ((&))
import Data.Pool.Introspection.Bean (PoolConf)
import Bornet.Api (BornetLinks, makeLinks)
import Bornet.Api.Server
import Bornet.Api.WholeServer
import Bornet.Repository
import Bornet.Repository.Sqlite qualified
import Bornet.Sqlite
import JsonConf
import JsonConf.YamlFile qualified
import Log
import Log.Backend.StandardOutput
import Network.Wai.Bean
import Network.Wai.Handler.Warp.Runner
import Sqlite (Connection)
import ThreadLocal

cauldron :: Cauldron Managed
cauldron =
  mconcat
    [ let makeJsonConf = JsonConf.YamlFile.make $ JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] JsonConf.YamlFile.useEnv
       in recipe @JsonConf $ ioEff_ $ pure makeJsonConf,
      recipe @Logger $ eff_ $ pure $ managed withStdOutLogger,
      recipe @SqlitePoolConf $ ioEff_ $ wire $ JsonConf.lookupSection @SqlitePoolConf "sqlite",
      recipe @PoolConf $ ioEff_ $ wire $ JsonConf.lookupSection @PoolConf "sqlite",
      recipe @SqlitePool $ eff_ $ wire $ \sconf pconf -> managed $ Bornet.Sqlite.makeSqlitePool sconf pconf,
      recipe @(ThreadLocal Connection) $ ioEff_ $ pure makeThreadLocal,
      -- IO Connection |=| val_ $ readThreadLocal @Connection <$> arg,
      recipe @(IO Connection) $ val_ $ wire $ readThreadLocal @Connection,
      recipe @BornetRepository $ val_ $ wire $ Bornet.Repository.Sqlite.make,
      recipe @BornetLinks $ ioEff_ $ pure makeLinks,
      recipe @BornetServer $
        Recipe
          { bare = val_ $ wire makeBornetServer,
            decos =
              [ val_ $ wire $ Bornet.Sqlite.hoistWithConnection Bornet.Api.Server.hoistBornetServer
              ]
          },
      recipe @StaticServeConf $ ioEff_ $ wire $ JsonConf.lookupSection @StaticServeConf "runner",
      recipe @Application $ val_ $ Bornet.Api.WholeServer.makeApplication <$> fmap Bornet.Api.Server.unwrap arg <*> arg,
      recipe @RunnerConf $ ioEff_ $ wire $ JsonConf.lookupSection @RunnerConf "runner",
      recipe @Runner $
        Recipe
          { bare = val_ $ wire makeRunner,
            decos =
              [ val_ $ wire $ \logger (conf :: RunnerConf) -> Network.Wai.Handler.Warp.Runner.decorate \action -> do
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
