{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Bornet.Api.Server
  ( BornetServer (..),
    unwrap,
    makeBornetServer,
    hoistBornetServer,
  )
where

import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson as Aeson
import Data.ByteString qualified
import Data.Coerce
import Data.Foldable qualified
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Int
import Data.List (List)
import Data.List qualified
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder qualified as LazyText
import Data.Text.Lazy.Builder.Int qualified as LazyText
import GHC.Stack (HasCallStack)
import Bornet.Api
import Bornet.Api.Server.Htmx
import Bornet.Model
import Bornet.Model as JornadaDetallada (JornadaDetallada (..))
import Bornet.Prelude
import Bornet.Repository
import Log
import Lucid
import Network.HTTP.Types.Header
import Network.URI (uriToString)
import Servant
import Prelude hiding (log)

newtype BornetServer = BornetServer {server :: Server Api}

unwrap :: BornetServer -> Server Api
unwrap BornetServer {server} = server

makeBornetServer ::
  Logger ->
  BornetLinks ->
  BornetRepository ->
  BornetServer
makeBornetServer logger BornetLinks {links} repository =
  BornetServer {server}
  where
    runLog = runLogT "server" logger defaultLogLevel
    server =
      Bornet
        { mainPage = handlerize do
            logInfo_ "Serving main page." & runLog
            horarios <- repository & listHorariosMetaOrdered
            geros <- repository & listGeros
            pure do
              pageWrap do
                nav_ do
                  pure ()
                main_ do
                  h1_ "Horarios"
                  ul_ do
                    Data.Foldable.for_ horarios \horarioMeta -> do
                      li_ do
                        a_ [uriHref_ (links.getHorario horarioMeta.horarioId)] do
                          toHtml horarioMeta.description
                  a_ [uriHref_ links.selectedExperimentsForAllHorarios] "Todo el año"
                  h1_ "Geros"
                  ul_ do
                    Data.Foldable.for_ geros \gero -> do
                      li_ do
                        a_ [uriHref_ (links.getGero gero.geroId)] do
                          toHtml gero.nombre,
          selectedExperimentsForAllHorarios = handlerize do
            logInfo_ "Serving selected experiments for all horarios." & runLog
            horarios <- repository & listHorariosMetaOrdered
            selectedPairs <- forM horarios \horarioMeta -> do
              experiments <- repository & listExperimentsByHorarioIdOrdered horarioMeta.horarioId
              let selected =
                    experiments
                      & filter (isJust . (.selectedAt))
                      & Data.List.sortOn (Down . (.selectedAt))
                      & listToMaybe
              case selected of
                Nothing -> pure (horarioMeta, Nothing)
                Just experiment -> do
                  detallado <- repository & getHorarioDetalladoForExperimentId experiment.experimentId
                  pure (horarioMeta, Just detallado)
            pure do
              pageWrap do
                nav_ do
                  a_ [href_ "/"] "Inicio"
                main_ do
                  renderSelectedExperimentsRow selectedPairs,
          getHorario = \horarioId -> handlerize do
            logInfo_ "Serving horario page." & runLog
            horario <- repository & getHorario horarioId
            experiments <- repository & listExperimentsByHorarioIdOrdered horarioId
            pure do
              pageWrap do
                nav_ do
                  a_ [href_ "/"] "Inicio"
                  ul_ do
                    Data.Foldable.for_ experiments \experiment -> do
                      li_ do
                        a_ [uriHref_ (links.experiments experiment.experimentId).getExperiment] do
                          renderExperimentDescription experiment
                main_ do
                  renderHorario horario
                  renderCreateExperiment (links.addExperiment horarioId),
          addExperiment = \horarioId description -> MkHandler do
            logInfo_ "Creating experiment" & runLog
            createdAt <- currentUnixEpoch
            experiment <- repository & insertExperiment Experiment {experimentId = dummyExperimentId, description, horarioId, createdAt, lockedAt = Nothing, selectedAt = Nothing}
            roles <- repository & listRoles <&> makeRoles
            horario <- repository & getHorario horarioId
            let horarioDetallado0 = makeHorarioDetallado0 experiment roles horario
            let jornadasDetalladas = horarioDetalladoToJornadasDetalladas experiment.experimentId horarioDetallado0
            logInfo_ "Before inserting jd" & runLog
            _ <- repository & insertExperimentJornadasDetalladas jornadasDetalladas
            logInfo_ "After inserting jornadas detalladas" & runLog
            -- \| https://hachyderm.io/@DiazCarrete/111841132226571708
            pure do
              Left
                err303
                  { errHeaders =
                      [ uriToLocationHeader $ (links.experiments experiment.experimentId).getExperiment
                      ]
                  },
          experiments = \experimentId ->
            let linksExperiment = links.experiments experimentId
                unlessLocked :: IO () -> IO (Html ())
                unlessLocked action = do
                  experiment <- repository & getExperimentById experimentId <&> fromJust
                  case experiment.lockedAt of
                    Just _ -> do
                      logInfo_ "Experiment is locked" & runLog
                      pure do
                        template_ do
                          div_ [id_ "error-panel", hxSwapOOB_ "true"] "El experimento está bloqueado"
                    Nothing -> do
                      action
                      pure (pure ())
             in ExperimentRoutes
                  { getExperiment = handlerize do
                      logInfo_ "Serving experiment" & runLog
                      horario <- repository & getHorarioDetalladoForExperimentId experimentId
                      logInfo_ "Serving experiment - after" & runLog
                      pure do
                        pageWrap do
                          nav_ do
                            renderExperimentDescription horario.experiment
                            a_ [uriHref_ (links.getHorario horario.experiment.horarioId)] do
                              "Volver"
                            a_ [uriHref_ (links.experiments experimentId).getExperimentApaisado] do
                              "Apaisado"
                            button_
                              [ hxDelete_ linksExperiment.deleteExperiment,
                                hxConfirm_ "¿Seguro que quieres eliminar este experimento?",
                                class_ "delete-button"
                              ]
                              "Borrar"
                          main_ do
                            renderErrorPanel
                            renderHorarioDetallado linksExperiment horario
                            let analysis = analyzeHorarioDetallado horario
                            let rolesRender = makeRolesRender horario
                            renderAnalysis horario.horarioDetallado.geros rolesRender analysis mempty
                            renderLeyenda
                            renderCreateExperiment linksExperiment.cloneExperiment
                            renderLockUnlockExperiment linksExperiment horario.experiment
                            renderSelectUnselectExperiment linksExperiment horario.experiment
                            pure (),
                    getExperimentApaisado = handlerize do
                      logInfo_ "Serving experiment apaisado" & runLog
                      horario <- repository & getHorarioDetalladoForExperimentId experimentId
                      allExperiments <- repository & listAllExperimentsOrdered
                      daysOrdered <- repository & listDaysOrdered
                      pure do
                        pageWrap do
                          nav_ do
                            renderExperimentDescription horario.experiment
                            a_ [uriHref_ (links.getHorario horario.experiment.horarioId)] do
                              "Volver"
                            a_ [uriHref_ (links.experiments experimentId).getExperiment] do
                              "Tabulado"
                          main_ do
                            renderErrorPanel
                            let analysis = analyzeHorarioDetallado horario
                            renderHorarioDetalladoApaisado linksExperiment horario analysis
                            let rolesRender = makeRolesRender horario
                            renderAnalysis horario.horarioDetallado.geros rolesRender analysis mempty
                            renderLeyenda
                            when (isNothing horario.experiment.lockedAt) do
                              renderImitateExperimentForm linksExperiment daysOrdered allExperiments
                            renderCreateExperiment linksExperiment.cloneExperiment
                            renderLockUnlockExperiment linksExperiment horario.experiment
                            renderSelectUnselectExperiment linksExperiment horario.experiment
                            pure (),
                    cloneExperiment = \description -> MkHandler do
                      logInfo_ "Cloning experiment" & runLog
                      createdAt <- currentUnixEpoch
                      Just originalExperiment <- repository & getExperimentById experimentId
                      jornadasDetalladas <- repository & listJornadasDetalladasByExperimentId experimentId
                      newExperiment <- repository & insertExperiment Experiment {experimentId = dummyExperimentId, description, horarioId = originalExperiment.horarioId, createdAt, lockedAt = Nothing, selectedAt = Nothing}
                      let newJornadasDetalladas = do
                            jornadasDetallada <- jornadasDetalladas
                            [jornadasDetallada {JornadaDetallada.experimentId = newExperiment.experimentId}]
                      _ <- repository & insertExperimentJornadasDetalladas newJornadasDetalladas
                      -- \| https://hachyderm.io/@DiazCarrete/111841132226571708
                      pure do
                        Left
                          err303
                            { errHeaders =
                                [ uriToLocationHeader $ (links.experiments newExperiment.experimentId).getExperiment
                                ]
                            },
                    deleteExperiment = handlerize do
                      logInfo_ "Deleting experiment" & runLog
                      horario <- repository & getHorarioDetalladoForExperimentId experimentId
                      repository & Bornet.Repository.deleteExperiment experimentId
                      pure $ addHeaderUri (links.getHorario horario.experiment.horarioId) (mempty :: Html ()),
                    lock = MkHandler do
                      logInfo_ "Locking experiment" & runLog
                      Just experiment <- repository & getExperimentById experimentId
                      now <- currentUnixEpoch
                      when (isNothing experiment.lockedAt) do
                        repository & updateExperiment experiment {lockedAt = Just now}
                      pure $ Left err303 {errHeaders = [uriToLocationHeader $ (links.experiments experimentId).getExperiment]},
                    unlock = MkHandler do
                      logInfo_ "Unlocking experiment" & runLog
                      Just experiment <- repository & getExperimentById experimentId
                      repository & updateExperiment experiment {lockedAt = Nothing}
                      pure $ Left err303 {errHeaders = [uriToLocationHeader $ (links.experiments experimentId).getExperiment]},
                    select = MkHandler do
                      logInfo_ "Selecting experiment" & runLog
                      Just experiment <- repository & getExperimentById experimentId
                      now <- currentUnixEpoch
                      when (isNothing experiment.selectedAt) do
                        repository & updateExperiment experiment {selectedAt = Just now}
                      pure $ Left err303 {errHeaders = [uriToLocationHeader $ (links.experiments experimentId).getExperiment]},
                    unselect = MkHandler do
                      logInfo_ "Unselecting experiment" & runLog
                      Just experiment <- repository & getExperimentById experimentId
                      repository & updateExperiment experiment {selectedAt = Nothing}
                      pure $ Left err303 {errHeaders = [uriToLocationHeader $ (links.experiments experimentId).getExperiment]},
                    permute = \permutation -> handlerize do
                      logInfo_ "Permuting" & runLog
                      lockedHtml <- unlessLocked do
                        roles <- repository & listRoles <&> makeRoles
                        when (isValidPermutation roles permutation) do
                          do
                            repository & permuteExperimentRow experimentId permutation
                            pure ()
                      horarioUpdated <- repository & getHorarioDetalladoForExperimentId experimentId
                      let geros = horarioUpdated.horarioDetallado.geros
                      let (day, weekdayName) = horarioUpdated.horarioDetallado.days & xlookupS "permute: looking up day2" permutation.day2
                      let rolesRender = makeRolesRender horarioUpdated
                      let turnosDiaDetallado = horarioUpdated.horarioDetallado.horario & xlookupS "permute: looking up shifts for day" day.dayId
                      let analysis = analyzeHorarioDetallado horarioUpdated
                      pure $ do
                        lockedHtml
                        renderDetailedInnerRow
                          linksExperiment
                          day
                          weekdayName
                          turnosDiaDetallado
                          rolesRender
                          geros
                        template_ do
                          renderAnalysis geros rolesRender analysis (hxSwapOOB_ "true"),
                    permuteApaisado = \permutation -> handlerize do
                      logInfo_ "Permuting (apaisado)" & runLog
                      lockedHtml <- unlessLocked do
                        roles <- repository & listRoles <&> makeRoles
                        when (isValidPermutation roles permutation) do
                          do
                            repository & permuteExperimentRow experimentId permutation
                            pure ()
                      horarioUpdated <- repository & getHorarioDetalladoForExperimentId experimentId
                      let geros = horarioUpdated.horarioDetallado.geros
                      let rolesRender = makeRolesRender horarioUpdated
                      let analysis = analyzeHorarioDetallado horarioUpdated
                      pure $ do
                        lockedHtml
                        renderHorarioDetalladoApaisado linksExperiment horarioUpdated analysis
                        template_ do
                          renderAnalysis geros rolesRender analysis (hxSwapOOB_ "true"),
                    permuteGeroRows = \geroSwap -> handlerize do
                      logInfo_
                        ( "Permuting gero rows (all days)"
                            <> Text.pack (Prelude.show experimentId)
                            <> " "
                            <> Text.pack (Prelude.show geroSwap)
                        )
                        & runLog
                      lockedHtml <- unlessLocked do
                        horario <- repository & getHorarioDetalladoForExperimentId experimentId
                        let geros = horario.horarioDetallado.geros
                        let gero1 = geros & xlookupS "permuteGeroRows: looking up gero1" geroSwap.gero1
                        let gero2 = geros & xlookupS "permuteGeroRows: looking up gero2" geroSwap.gero2
                        -- Only proceed if both geros belong to the same group
                        when (gero1.groupId == gero2.groupId) do
                          let geroRolesByDay = buildGeroRolesByDay horario
                          let roles1ByDay = Map.findWithDefault Map.empty geroSwap.gero1 geroRolesByDay
                          let roles2ByDay = Map.findWithDefault Map.empty geroSwap.gero2 geroRolesByDay
                          -- Find days where both geros work
                          let commonDayIds = Set.intersection (Map.keysSet roles1ByDay) (Map.keysSet roles2ByDay)
                          -- For each common day, swap roles
                          Data.Foldable.for_ commonDayIds \dayId -> do
                            let role1 = roles1ByDay & xlookupS "permuteGeroRows: role1" dayId
                            let role2 = roles2ByDay & xlookupS "permuteGeroRows: role2" dayId
                            let permutation =
                                  Permutation
                                    { day1 = dayId,
                                      role1 = role1,
                                      gero1 = geroSwap.gero1,
                                      day2 = dayId,
                                      role2 = role2,
                                      gero2 = geroSwap.gero2
                                    }
                            repository & permuteExperimentRow experimentId permutation
                      -- Re-fetch to get updated state
                      horarioUpdated <- repository & getHorarioDetalladoForExperimentId experimentId
                      let gerosUpdated = horarioUpdated.horarioDetallado.geros
                      let rolesRender = makeRolesRender horarioUpdated
                      let analysis = analyzeHorarioDetallado horarioUpdated
                      pure $ do
                        lockedHtml
                        renderHorarioDetalladoApaisado linksExperiment horarioUpdated analysis
                        template_ do
                          renderAnalysis gerosUpdated rolesRender analysis (hxSwapOOB_ "true"),
                    imitateExperiment = \imitation -> MkHandler do
                      logInfo_ ("Imitating experiment " <> Text.pack (show imitation)) & runLog
                      _ <- unlessLocked do
                        roles <- repository & listRoles <&> makeRoles
                        sourceHorario <- repository & getHorarioDetalladoForExperimentId imitation.source
                        targetHorario <- repository & getHorarioDetalladoForExperimentId experimentId
                        let imitationDetallado = imitation {source = sourceHorario}
                        -- Additional sanity check.
                        let permutations =
                              let candidates = generateImitationPermutations imitationDetallado targetHorario
                               in candidates & filter (isValidPermutation roles)
                        let permsByDay = Map.fromListWith (+) [(p.day1, 1 :: Int) | p <- permutations]
                        logInfo_ ("Permutations by day: " <> Text.pack (show permsByDay)) & runLog
                        Data.Foldable.for_ permutations \permutation ->
                          repository & permuteExperimentRow experimentId permutation
                      pure do
                        Left
                          err303
                            { errHeaders =
                                [ uriToLocationHeader $ (links.experiments experimentId).getExperimentApaisado
                                ]
                            }
                  },
          getGero = \geroId -> handlerize do
            logInfo_ "Serving gero page." & runLog
            Just gero <- repository & getGeroById geroId
            horarios <- repository & listHorariosMetaOrdered
            -- For each horario, get days and jornadas for this gero
            horarioRows <- forM horarios \horarioMeta -> do
              allDays <- repository & listDaysOrdered
              jornadas <- repository & listJornadasByHorarioId horarioMeta.horarioId
              let geroJornadas = filter (\j -> j.geroId == geroId) jornadas
                  referencedDayIds = Set.fromList $ (.dayId) <$> jornadas
                  daysForHorario = filter (\d -> d.dayId `Set.member` referencedDayIds) allDays
                  -- Map from day orden (1-31) to the day
                  daysByOrden = Map.fromList [(d.orden, d) | d <- daysForHorario]
                  -- Map from dayId to shiftId for this gero
                  geroShiftByDay = Map.fromList [(j.dayId, j.shiftId) | j <- geroJornadas]
              pure (horarioMeta, daysByOrden, geroShiftByDay)
            pure do
              pageWrap do
                nav_ do
                  a_ [href_ "/"] "Inicio"
                main_ do
                  h1_ $ toHtml gero.nombre
                  renderGeroTable horarioRows
        }

uriToLocationHeader :: URI -> (HeaderName, Data.ByteString.ByteString)
uriToLocationHeader uri =
  (hLocation, toHeader $ uriToString id uri "")

handlerize :: IO r -> Handler r
handlerize action = coerce do fmap (Right @ServerError) action

hoistBornetServer :: (forall x. IO x -> IO x) -> BornetServer -> BornetServer
hoistBornetServer f BornetServer {server = _server} =
  BornetServer {server = hoistServer (Proxy @Api) h _server}
  where
    h :: forall r. Handler r -> Handler r
    h (MkHandler action) = MkHandler (f action)

renderHorario :: Horario -> Html ()
renderHorario theHorario = do
  table_ do
    caption_ $ "Horario " <> toHtml theHorario.meta.description
    -- https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/colgroup
    colgroup_ do
      col_ []
      col_ [classMorningShift]
      col_ [classAfternoonShift]
    thead_ do
      tr_ do
        td_ ""
        th_ [scope_ "col"] "M"
        th_ [scope_ "col"] "T"
    tbody_ do
      theHorario & forDaysOrdered \day weekdayName -> do
        let turnosDia = theHorario.horario & xlookupS "renderHorario: looking up shifts for day" day.dayId
        renderDayShifts theHorario.geros day weekdayName turnosDia

renderDayShifts :: Geros -> Day -> WeekdayName -> DayShifts -> Html ()
renderDayShifts geros day weekdayName turnosDia = do
  tr_ do
    th_ [scope_ "row"] do
      toHtml $ Text.pack (show day.orden) <> " " <> weekdayNameToText weekdayName
    td_ do
      let geroIds = turnosDia & xlookupS ("renderDayShifts: looking up morning shift geros" ++ show day ++ " ") morningShift
      renderGeroNames geros geroIds
    td_ do
      let geroIds = turnosDia & xlookupS ("renderDayShifts: looking up afternoon shift geros" ++ show day ++ " ") afternoonShift
      renderGeroNames geros geroIds

renderGeroNames :: (Foldable f) => Geros -> f GeroId -> Html ()
renderGeroNames geros (Data.Foldable.toList -> geroIds) = do
  toHtml $ mconcat $ Data.List.intersperse ", " geroNames
  where
    geroNames :: [Text]
    geroNames = geroIds <&> \geroId -> (geros & xlookupS "renderGeroNames: looking up gero by id" geroId).nombre

renderHorarioDetallado :: MkLink (NamedRoutes ExperimentRoutes) URI -> HorarioDetallado -> Html ()
renderHorarioDetallado links horario = do
  let rolesRender = makeRolesRender horario
  table_ [hxExt_ "drag"] do
    caption_ $ "Horario con tareas " <> toHtml horario.horarioDetallado.meta.description
    renderRoleTableHead rolesRender
    tbody_ do
      horario.horarioDetallado & forDaysOrdered \day weekdayName -> do
        let turnosDiaDetallado = horario.horarioDetallado.horario & xlookupS "renderHorarioDetallado: looking up detailed shifts for day" day.dayId
        tr_ do
          renderDetailedInnerRow links day weekdayName turnosDiaDetallado rolesRender horario.horarioDetallado.geros

renderHorarioDetalladoApaisado :: ExperimentRoutes (AsLink URI) -> HorarioDetallado -> HorarioDetalladoAnalysis -> Html ()
renderHorarioDetalladoApaisado links =
  renderHorarioDetalladoApaisadoGeneral
    (id_ "apaisado")
    (Just (links, mconcat [hxTarget_ "#apaisado", hxSwap_ "outerHTML"]))

-- | Transposed view: geros as rows, days as columns, cells show role names
-- Has parameters that control if we should enable drag and drop or not.
-- Useful when we want to render multiple purely informational tables in the same page.
renderHorarioDetalladoApaisadoGeneral :: Attributes -> Maybe (ExperimentRoutes (AsLink URI), Attributes) -> HorarioDetallado -> HorarioDetalladoAnalysis -> Html ()
renderHorarioDetalladoApaisadoGeneral containerId mlinks horario analysis = do
  let geroRolesByDay = buildGeroRolesByDay horario
  table_ [containerId, class_ "apaisado", hxExt_ "drag"] do
    caption_ $ "Horario con tareas " <> toHtml horario.horarioDetallado.meta.description
    thead_ do
      tr_ do
        th_ ""
        Data.Foldable.for_ horario.horarioDetallado.daysOrdered \(day, weekdayName) -> do
          th_ [scope_ "col"] do
            toHtml $ Text.pack (show day.orden) <> " " <> weekdayNameToText weekdayName
    tbody_ do
      Data.Foldable.for_ (horario.horarioDetallado.geros & Map.elems & Data.List.sortOn (\g -> (g.groupId, g.geroId))) \gero -> do
        tr_ do
          th_
            ( case mlinks of
                Nothing -> mempty
                Just (links, containerTarget) ->
                  [ scope_ "row",
                    draggable_ "true",
                    containerTarget,
                    hxDrag_ $
                      object
                        [ "gero1" Aeson..= gero.geroId
                        ],
                    hxDrop_ $
                      object
                        [ "gero2" Aeson..= gero.geroId
                        ],
                    hxDropMethod_ "POST",
                    hxDropAction_ links.permuteGeroRows
                  ]
            )
            do
              toHtml gero.nombre
          Data.Foldable.for_ horario.horarioDetallado.daysOrdered \(day, _weekdayName) -> do
            case Map.lookup gero.geroId geroRolesByDay >>= Map.lookup day.dayId of
              Nothing -> td_ [class_ "not_today"] do
                pure ()
              Just roleId -> do
                let role = horario.roles & xlookupS "renderHorarioDetalladoApaisado: looking up role" roleId
                let geroFreqs = analysis & xlookupS "renderHorarioDetalladoApaisado: looking up analysis for gero" gero.geroId
                let freq = geroFreqs & xlookupS "renderHorarioDetalladoApaisado: looking up freq for role" roleId
                td_
                  [ mconcat case mlinks of
                      Nothing -> []
                      Just (links, containerTarget) ->
                        [ draggable_ "true",
                          containerTarget,
                          hxDrag_ $
                            object
                              [ "day1" Aeson..= day.dayId,
                                "role1" Aeson..= role.roleId,
                                "gero1" Aeson..= gero.geroId
                              ],
                          hxDrop_ $
                            object
                              [ "day2" Aeson..= day.dayId,
                                "role2" Aeson..= role.roleId,
                                "gero2" Aeson..= gero.geroId
                              ],
                          hxDropMethod_ "POST",
                          hxDropAction_ links.permuteApaisado
                        ],
                    if role.shiftId == morningShift
                      then classMorningShift
                      else classAfternoonShift,
                    freqClass freq
                  ]
                  do
                    toHtml role.nombre

-- | Build an index: GeroId -> DayId -> RoleId
buildGeroRolesByDay :: HorarioDetallado -> Map.Map GeroId (Map.Map DayId RoleId)
buildGeroRolesByDay horario =
  Map.fromListWith Map.union $ do
    (dayId, shifts) <- Map.toList horario.horarioDetallado.horario
    (_shiftId, rolesMap) <- Map.toList shifts
    (roleId, geroId) <- Map.toList rolesMap
    return (geroId, Map.singleton dayId roleId)

renderAnalysis :: Geros -> RolesRender -> HorarioDetalladoAnalysis -> Attributes -> Html ()
renderAnalysis geros rolesRender@RolesRender {rolesMañanaOrdered, rolesTardeOrdered} analysis tableAttributes = do
  -- toHtml $ show analysis
  table_ [id_ "analysis", tableAttributes] do
    caption_ "Totales de tareas por empleada"
    renderRoleTableHead rolesRender
    tbody_ do
      Data.Foldable.for_ (geros & Map.elems & Data.List.sortOn (\g -> (g.groupId, g.geroId))) \gero -> do
        let freqs = analysis & xlookupS "renderAnalysis: looking up analysis for gero" gero.geroId
        tr_ do
          th_ [scope_ "row"] do
            toHtml gero.nombre
          Data.Foldable.for_ rolesMañanaOrdered \role -> do
            let freq = fromMaybe 0 $ Map.lookup role.roleId freqs
            td_ [freqClass freq] do
              toHtml $ show freq
          Data.Foldable.for_ rolesTardeOrdered \role -> do
            let freq = fromMaybe 0 $ Map.lookup role.roleId freqs
            td_ [freqClass freq] do
              toHtml $ show freq

renderErrorPanel :: Html ()
renderErrorPanel = div_ [id_ "error-panel"] ""

renderDetailedInnerRow ::
  (HasCallStack) =>
  ExperimentRoutes (AsLink URI) ->
  Day ->
  WeekdayName ->
  DetailedDayShifts ->
  RolesRender ->
  Geros ->
  Html ()
renderDetailedInnerRow links day weekdayName turnosDiaDetallado RolesRender {rolesMañanaOrdered, rolesTardeOrdered} geros = do
  th_ [scope_ "row"] do
    toHtml $ Text.pack (show day.orden) <> " " <> weekdayNameToText weekdayName
  let rolesDiaMañana = turnosDiaDetallado & xlookupS "renderDetailedInnerRow: looking up morning shift roles" morningShift
  Data.Foldable.for_ rolesMañanaOrdered \role -> do
    case Map.lookup role.roleId rolesDiaMañana of
      Nothing -> renderMissingGero
      Just geroId -> do
        let gero = geros & xlookupS "renderDetailedInnerRow: looking up gero by id (morning)" geroId
        renderDraggableGero links day role gero
  let rolesDiaTarde = turnosDiaDetallado & xlookupS "renderDetailedInnerRow: looking up afternoon shift roles" afternoonShift
  Data.Foldable.for_ rolesTardeOrdered \role -> do
    case Map.lookup role.roleId rolesDiaTarde of
      Nothing -> renderMissingGero
      Just geroId -> do
        let gero = geros & xlookupS "renderDetailedInnerRow: looking up gero by id (afternoon)" geroId
        renderDraggableGero links day role gero

renderSelectedExperimentsRow :: [(HorarioMeta, Maybe HorarioDetallado)] -> Html ()
renderSelectedExperimentsRow metas = do
  div_ [class_ "selected-experiments-row"] do
    Data.Foldable.for_ metas \(horarioMeta, mDetallado) -> do
      div_ [class_ "selected-experiment-item"] do
        h2_ $ toHtml horarioMeta.description
        case mDetallado of
          Nothing -> do
            p_ "Sin experimento seleccionado"
          Just horario -> do
            let analysis = analyzeHorarioDetallado horario
            renderHorarioDetalladoApaisadoGeneral mempty Nothing horario analysis

pageWrap :: Html () -> Html ()
pageWrap contents = do
  doctypehtml_ do
    head_ do
      title_ "Bornet"
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/styles.css"]
      script_ [type_ "text/javascript", src_ "/static/htmx.min.js"] ("" :: Text)
      script_ [type_ "text/javascript", src_ "/static/hx-drag.js"] ("" :: Text)
    -- We're using htmx instead.
    -- script_ [type_ "text/javascript", src_ "/static/drag-n-drop.js"] ("" :: Text)
    body_ do
      contents

renderMissingGero :: Html ()
renderMissingGero = td_ [class_ "missing_gero"] $ "X"

renderDraggableGero :: MkLink (NamedRoutes ExperimentRoutes) URI -> Day -> Role -> Gero -> Html ()
renderDraggableGero links day role gero = do
  td_
    [ draggable_ "true",
      hxTarget_ "closest tr",
      hxDrag_ $
        object
          [ "day1" Aeson..= day.dayId,
            "role1" Aeson..= role.roleId,
            "gero1" Aeson..= gero.geroId
          ],
      hxDrop_ $
        object
          [ "day2" Aeson..= day.dayId,
            "role2" Aeson..= role.roleId,
            "gero2" Aeson..= gero.geroId
          ],
      hxDropMethod_ "POST",
      hxDropAction_ $ links.permute
    ]
    do
      toHtml $ gero.nombre

renderCreateExperiment :: URI -> Html ()
renderCreateExperiment uri = do
  form_ [method_ "post", action_ (uriText uri)] do
    label_ [for_ "form_description"] "Crear plan"
    input_
      [ type_ "text",
        id_ "form_description",
        name_ "description",
        required_ "true",
        minlength_ "1",
        size_ "20",
        rows_ "5"
      ]
    input_ [type_ "submit", value_ "Send"]

renderExperimentDescription :: Experiment -> Html ()
renderExperimentDescription experiment = do
  span_ do
    toHtml experiment.description
    when (isJust experiment.lockedAt) "🔒"
    when (isJust experiment.selectedAt) "👑"

renderLockUnlockExperiment :: ExperimentRoutes (AsLink URI) -> Experiment -> Html ()
renderLockUnlockExperiment linksExperiment experiment = do
  case experiment.lockedAt of
    Nothing ->
      form_ [method_ "post", action_ (uriText linksExperiment.lock)] do
        input_ [type_ "submit", value_ "Bloquear"]
    Just _ ->
      form_ [method_ "post", action_ (uriText linksExperiment.unlock)] do
        input_ [type_ "submit", value_ "Desbloquear"]

renderSelectUnselectExperiment :: ExperimentRoutes (AsLink URI) -> Experiment -> Html ()
renderSelectUnselectExperiment linksExperiment experiment = do
  case experiment.selectedAt of
    Nothing ->
      form_ [method_ "post", action_ (uriText linksExperiment.select)] do
        input_ [type_ "submit", value_ "Seleccionar"]
    Just _ ->
      form_ [method_ "post", action_ (uriText linksExperiment.unselect)] do
        input_ [type_ "submit", value_ "Deseleccionar"]

renderImitateExperimentForm :: ExperimentRoutes (AsLink URI) -> [Day] -> [Experiment] -> Html ()
renderImitateExperimentForm linksExperiment allDays allExperiments = do
  let targetDays = allDays
  form_ [method_ "post", action_ (uriText linksExperiment.imitateExperiment)] do
    fieldset_ do
      legend_ "Imitar otro experimento"
      label_ [for_ "imitate_source"] "Experimento fuente"
      select_ [name_ "source", id_ "imitate_source", required_ "true"] do
        Data.Foldable.for_ allExperiments \experiment -> do
          option_ [value_ (toUrlPiece experiment.experimentId)] do
            renderExperimentDescription experiment
      label_ [for_ "imitate_sourceStartDay"] "Día inicio fuente"
      select_ [name_ "sourceStartDay", id_ "imitate_sourceStartDay", required_ "true"] do
        Data.Foldable.for_ targetDays \day -> do
          option_ [value_ (toUrlPiece day.dayId)] do
            toHtml $ Text.pack (show day.orden)
      label_ [for_ "imitate_sourceEndDay"] "Día fin fuente"
      select_ [name_ "sourceEndDay", id_ "imitate_sourceEndDay", required_ "true"] do
        Data.Foldable.for_ targetDays \day -> do
          option_ [value_ (toUrlPiece day.dayId)] do
            toHtml $ Text.pack (show day.orden)
      label_ [for_ "imitate_targetStartDay"] "Día inicio destino"
      select_ [name_ "targetStartDay", id_ "imitate_targetStartDay", required_ "true"] do
        Data.Foldable.for_ targetDays \day -> do
          option_ [value_ (toUrlPiece day.dayId)] do
            toHtml $ Text.pack (show day.orden)
      input_ [type_ "submit", value_ "Imitar"]

renderLeyenda :: Html ()
renderLeyenda = do
  dl_ [class_ "leyenda"] do
    dt_ [classMorningShift] "M1"
    dd_ "comedor"
    dt_ [classMorningShift] "M2"
    dd_ "sala"
    dt_ [classMorningShift] "M3"
    dd_ "apoyo sala"
    dt_ [classMorningShift] "M4"
    dd_ "turbo"
    dt_ [classMorningShift] "M5"
    dd_ "apoyo comedor"
    dt_ [classMorningShift] "M6"
    dd_ "correturnos"
    dt_ [classAfternoonShift] "T1"
    dd_ "comedor"
    dt_ [classAfternoonShift] "T2"
    dd_ "sala"
    dt_ [classAfternoonShift] "T3"
    dd_ "pasillo"

-- | Defined here, instead of in the model,
-- because role display order isn't really relevant to the model.
data RolesRender = RolesRender
  { rolesMañanaOrdered :: List Role,
    rolesTardeOrdered :: List Role
  }

makeRolesRender :: HorarioDetallado -> RolesRender
makeRolesRender horario = do
  let rolesByShift = makeRolesByShift horario.roles
  let rolesMañana = rolesByShift & xlookupS "makeRolesRender: looking up morning shift roles" morningShift
  let rolesMañanaOrdered = rolesMañana & Map.elems & Data.List.sortOn (.nombre)
  let rolesTarde = rolesByShift & xlookupS "makeRolesRender: looking up afternoon shift roles" afternoonShift
  let rolesTardeOrdered = rolesTarde & Map.elems & Data.List.sortOn (.nombre)
  RolesRender {rolesMañanaOrdered, rolesTardeOrdered}

renderRoleTableHead :: RolesRender -> Html ()
renderRoleTableHead rolesRender = do
  -- https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/colgroup
  colgroup_ do
    col_ []
    col_ [classMorningShift, span_ $ toStrict $ LazyText.toLazyText $ LazyText.decimal $ Data.List.length rolesRender.rolesMañanaOrdered]
    col_ [classAfternoonShift, span_ $ toStrict $ LazyText.toLazyText $ LazyText.decimal $ Data.List.length rolesRender.rolesTardeOrdered]
  thead_ do
    tr_ do
      td_ ""
      Data.Foldable.for_ rolesRender.rolesMañanaOrdered \rol -> do
        th_ [scope_ "col"] (toHtml rol.nombre)
      Data.Foldable.for_ rolesRender.rolesTardeOrdered \rol -> do
        th_ [scope_ "col"] (toHtml rol.nombre)

classMorningShift :: Attributes
classMorningShift = class_ "morning-shift"

classAfternoonShift :: Attributes
classAfternoonShift = class_ "afternoon-shift"

freqClass :: Int -> Attributes
freqClass freq
  | freq > 4 = class_ "freq-high"
  | freq < 2 = class_ "freq-low"
  | otherwise = mempty

-- | Render a table for a single gero showing their shifts across all months
-- Rows = horarios (months), Columns = days 1-31
renderGeroTable :: [(HorarioMeta, Map.Map Int64 Day, Map.Map DayId ShiftId)] -> Html ()
renderGeroTable horarioRows = do
  table_ [class_ "gero_calendar"] do
    thead_ do
      tr_ do
        th_ ""
        Data.Foldable.for_ [1 .. 31 :: Int64] \dayNum -> do
          th_ [scope_ "col"] $ toHtml $ show dayNum
    tbody_ do
      Data.Foldable.for_ horarioRows \(horarioMeta, daysByOrden, geroShiftByDay) -> do
        tr_ do
          th_ [scope_ "row"] $ toHtml horarioMeta.description
          Data.Foldable.for_ [1 .. 31 :: Int64] \dayNum -> do
            case Map.lookup dayNum daysByOrden of
              Nothing ->
                -- Day doesn't exist for this month
                td_ [class_ "no_month_day"] mempty
              Just day ->
                case Map.lookup day.dayId geroShiftByDay of
                  Nothing ->
                    -- Day exists but gero doesn't work
                    td_ [class_ "day_free"] mempty
                  Just shiftId
                    | shiftId == morningShift ->
                        td_ [classMorningShift] "M"
                    | shiftId == afternoonShift ->
                        td_ [classAfternoonShift] "T"
                    | otherwise ->
                        td_ "?"

addHeaderUri :: (AddHeader [Optional, Strict] h Text orig new) => URI -> orig -> new
addHeaderUri uri = addHeader (uriText uri)
