{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Bornet.Repository where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Bornet.Model
import Bornet.Prelude

data BornetRepository = BornetRepository
  { _getGeroById :: GeroId -> IO (Maybe Gero),
    _listGeros :: IO [Gero],
    _listShifts :: IO [Shift],
    _listRoles :: IO [Role],
    _listDaysOrdered :: IO [Day],
    _listJornadasByHorarioId :: HorarioId -> IO [Jornada],
    _getHorarioMetaById :: HorarioId -> IO (Maybe HorarioMeta),
    _listHorariosMetaOrdered :: IO [HorarioMeta],
    _listAllExperimentsOrdered :: IO [Experiment],
    _listExperimentsByHorarioIdOrdered :: HorarioId -> IO [Experiment],
    _getExperimentById :: ExperimentId -> IO (Maybe Experiment),
    _listJornadasDetalladasByExperimentId :: ExperimentId -> IO [JornadaDetallada],
    -- | Returns the same 'Experiment', with a newly created 'ExperimentId'.
    _insertExperiment :: Experiment -> IO Experiment,
    _updateExperiment :: Experiment -> IO (),
    _insertExperimentJornadasDetalladas :: [JornadaDetallada] -> IO (),
    _permuteExperimentRow :: ExperimentId -> Permutation -> IO (),
    _deleteExperiment :: ExperimentId -> IO ()
  }

getGeroById :: GeroId -> BornetRepository -> IO (Maybe Gero)
getGeroById geroId BornetRepository {_getGeroById} = _getGeroById geroId

listGeros :: BornetRepository -> IO [Gero]
listGeros BornetRepository {_listGeros} = _listGeros

listShifts :: BornetRepository -> IO [Shift]
listShifts BornetRepository {_listShifts} = _listShifts

listRoles :: BornetRepository -> IO [Role]
listRoles BornetRepository {_listRoles} = _listRoles

listDaysOrdered :: BornetRepository -> IO [Day]
listDaysOrdered BornetRepository {_listDaysOrdered} = _listDaysOrdered

listJornadasByHorarioId :: HorarioId -> BornetRepository -> IO [Jornada]
listJornadasByHorarioId horarioId BornetRepository {_listJornadasByHorarioId} = _listJornadasByHorarioId horarioId

getHorarioMetaById :: HorarioId -> BornetRepository -> IO (Maybe HorarioMeta)
getHorarioMetaById horarioId BornetRepository {_getHorarioMetaById} = _getHorarioMetaById horarioId

listHorariosMetaOrdered :: BornetRepository -> IO [HorarioMeta]
listHorariosMetaOrdered BornetRepository {_listHorariosMetaOrdered} = _listHorariosMetaOrdered

listAllExperimentsOrdered :: BornetRepository -> IO [Experiment]
listAllExperimentsOrdered BornetRepository {_listAllExperimentsOrdered} = _listAllExperimentsOrdered

listExperimentsByHorarioIdOrdered :: HorarioId -> BornetRepository -> IO [Experiment]
listExperimentsByHorarioIdOrdered horarioId BornetRepository {_listExperimentsByHorarioIdOrdered} = _listExperimentsByHorarioIdOrdered horarioId

getExperimentById :: ExperimentId -> BornetRepository -> IO (Maybe Experiment)
getExperimentById experimentId BornetRepository {_getExperimentById} = _getExperimentById experimentId

listJornadasDetalladasByExperimentId :: ExperimentId -> BornetRepository -> IO [JornadaDetallada]
listJornadasDetalladasByExperimentId experimentId BornetRepository {_listJornadasDetalladasByExperimentId} = _listJornadasDetalladasByExperimentId experimentId

insertExperiment :: Experiment -> BornetRepository -> IO Experiment
insertExperiment experiment BornetRepository {_insertExperiment} = _insertExperiment experiment

updateExperiment :: Experiment -> BornetRepository -> IO ()
updateExperiment experiment BornetRepository {_updateExperiment} = _updateExperiment experiment

insertExperimentJornadasDetalladas :: [JornadaDetallada] -> BornetRepository -> IO ()
insertExperimentJornadasDetalladas jornadas BornetRepository {_insertExperimentJornadasDetalladas} = _insertExperimentJornadasDetalladas jornadas

permuteExperimentRow :: ExperimentId -> Permutation -> BornetRepository -> IO ()
permuteExperimentRow experimentId permutation BornetRepository {_permuteExperimentRow} = _permuteExperimentRow experimentId permutation

deleteExperiment :: ExperimentId -> BornetRepository -> IO ()
deleteExperiment experimentId BornetRepository {_deleteExperiment} = _deleteExperiment experimentId

getHorario :: HorarioId -> BornetRepository -> IO Horario
getHorario horarioId repository = do
  Just meta <- repository & getHorarioMetaById horarioId
  geros <- repository & listGeros <&> makeGeros
  shifts <- repository & listShifts <&> makeShifts
  allDaysOrdered <- repository & listDaysOrdered
  jornadas <- repository & listJornadasByHorarioId horarioId
  let referencedDayIds = Set.fromList $ (.dayId) <$> jornadas
      daysOrdered =
        allDaysOrdered
          & filter (\d -> d.dayId `Set.member` referencedDayIds)
          & decorateWithWeekdayNames meta.weekdayStart
      days = makeDays daysOrdered
      -- Ensure both morning and afternoon shifts exist for each day, even if empty
      defaultShifts = Map.fromList [(morningShift, Set.empty), (afternoonShift, Set.empty)]
      horario =
        Map.map (`Map.union` defaultShifts) $
          Map.fromListWith (Map.unionWith Set.union) $ do
            Jornada {dayId, shiftId, geroId} <- jornadas
            return (dayId, Map.singleton shiftId (Set.singleton geroId))
  pure Horario {meta, days, daysOrdered, geros, shifts, horario}

getHorarioDetalladoForExperimentId :: ExperimentId -> BornetRepository -> IO HorarioDetallado
getHorarioDetalladoForExperimentId experimentId repository = do
  Just experiment <- repository & getExperimentById experimentId
  Just meta <- repository & getHorarioMetaById experiment.horarioId
  geros <- repository & listGeros <&> makeGeros
  shifts <- repository & listShifts <&> makeShifts
  roles <- repository & listRoles <&> makeRoles
  allDaysOrdered <- repository & listDaysOrdered
  jornadasDetalladas <- repository & listJornadasDetalladasByExperimentId experimentId
  let referencedDayIds = Set.fromList $ (.dayId) <$> jornadasDetalladas
      daysOrdered =
        allDaysOrdered
          & filter (\d -> d.dayId `Set.member` referencedDayIds)
          & decorateWithWeekdayNames meta.weekdayStart
      days = makeDays daysOrdered
  let -- Ensure both morning and afternoon shifts exist for each day, even if empty
      defaultShifts = Map.fromList [(morningShift, Map.empty), (afternoonShift, Map.empty)]
  pure
    HorarioDetallado
      { experiment,
        roles,
        rolesByShift = makeRolesByShift roles,
        horarioDetallado =
          Horario
            { meta,
              days,
              daysOrdered,
              geros,
              shifts,
              horario =
                Map.map (`Map.union` defaultShifts) $
                  Map.fromListWith (Map.unionWith Map.union) do
                    JornadaDetallada {dayId, geroId, roleId} <- jornadasDetalladas
                    let shiftId = (roles & xlookupS "getHorarioDetalladoForExperimentId: looking up role to get shift" roleId).shiftId
                    return (dayId, Map.singleton shiftId (Map.singleton roleId geroId))
            }
      }
