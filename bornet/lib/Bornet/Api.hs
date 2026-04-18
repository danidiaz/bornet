{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Bornet.Api
  ( Api,
    Bornet (..),
    ExperimentApi,
    ExperimentRoutes (..),
    -- IncomingComment (..),
    BornetLinks (..),
    makeLinks,
  )
where

import Data.Function ((&))
import Data.Proxy
import Data.Text (Text)
import GHC.Generics (Generic)
import Bornet.Model qualified
import Lucid
import Network.URI
import Servant.API
import Servant.API.ContentTypes.Lucid
import Servant.Links

type Api = NamedRoutes Bornet

data Bornet mode = Bornet
  { mainPage ::
      mode
        :- Get '[HTML] (Html ()),
    selectedExperimentsForAllHorarios ::
      mode
        :- "selected-experiments-all-horarios"
          :> Get '[HTML] (Html ()),
    getHorario ::
      mode
        :- "horario"
          :> Capture "horarioId" Bornet.Model.HorarioId
          :> Get '[HTML] (Html ()),
    addExperiment ::
      mode
        :- "horario"
          :> Capture "horarioId" Bornet.Model.HorarioId
          :> ReqBody '[FormUrlEncoded] Bornet.Model.ExperimentDescription
          :> Post '[HTML] (Html ()),
    experiments ::
      mode
        :- "experiment"
          :> ExperimentApi,
    getGero ::
      mode
        :- "gero"
          :> Capture "geroId" Bornet.Model.GeroId
          :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)

type ExperimentApi =
  Capture "experimentId" Bornet.Model.ExperimentId
    :> NamedRoutes ExperimentRoutes

data ExperimentRoutes mode = ExperimentRoutes
  { getExperiment ::
      mode
        :- Get '[HTML] (Html ()),
    getExperimentApaisado ::
      mode
        :- "apaisado"
          :> Get '[HTML] (Html ()),
    cloneExperiment ::
      mode
        :- ReqBody '[FormUrlEncoded] Bornet.Model.ExperimentDescription
          :> Post '[HTML] (Html ()),
    deleteExperiment ::
      mode
        :- Delete '[HTML] (Headers '[Header "HX-Redirect" Text] (Html ())),
    lock ::
      mode
        :- "lock"
          :> Post '[HTML] (Html ()),
    unlock ::
      mode
        :- "unlock"
          :> Post '[HTML] (Html ()),
    select ::
      mode
        :- "select"
          :> Post '[HTML] (Html ()),
    unselect ::
      mode
        :- "unselect"
          :> Post '[HTML] (Html ()),
    permute ::
      mode
        :- "permute"
          :> ReqBody '[FormUrlEncoded] Bornet.Model.Permutation
          :> Post '[HTML] (Html ()),
    permuteApaisado ::
      mode
        :- "apaisado"
          :> "permute"
          :> ReqBody '[FormUrlEncoded] Bornet.Model.Permutation
          :> Post '[HTML] (Html ()),
    -- Permute rows between two geros for all days in the experiment.
    -- the geros must belong to the same group.
    permuteGeroRows ::
      mode
        :- "apaisado"
          :> "permute-gero-rows"
          :> ReqBody '[FormUrlEncoded] Bornet.Model.GeroSwap
          :> Post '[HTML] (Html ()),
    imitateExperiment ::
      mode
        :- "imitate"
          :> ReqBody '[FormUrlEncoded] (Bornet.Model.ExperimentImitation Bornet.Model.ExperimentId)
          :> Post '[HTML] (Html ())
  }
  deriving stock (Generic)

newtype BornetLinks = BornetLinks {links :: Bornet (AsLink URI)}

-- | Create a links struct with absolute URIs
-- https://hachyderm.io/@DiazCarrete/111841132226571708
-- https://www.youtube.com/watch?v=KC64Ymo63hQ
makeLinks :: IO BornetLinks
makeLinks = do
  root <- parseRelativeReference "/" & maybe (fail "Could not create root URI") pure
  let links = allLinks' (\r -> linkURI r `relativeTo` root) (Proxy @Api)
  pure BornetLinks {links}
