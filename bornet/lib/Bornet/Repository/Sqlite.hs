{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bornet.Repository.Sqlite where

import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.Text
import Bornet.Model
import Bornet.Model as Experiment (Experiment (..))
import Bornet.Repository
import Log
import Sqlite.Query

make ::
  Logger ->
  IO Connection ->
  BornetRepository
make logger askConn =
  BornetRepository
    { _getGeroById = \geroId -> do
        logInfo_ "Getting Gero by ID." & runLog
        conn <- askConn
        results <-
          Sqlite.Query.select
            conn
            "select gero_id, nombre, group_id from gero where gero_id = ?"
            [geroId]
        pure $ listToMaybe results,
      _listGeros = do
        logInfo_ "Listing Geros." & runLog
        conn <- askConn
        Sqlite.Query.select_
          conn
          "select gero_id, nombre, group_id from gero",
      _listShifts = do
        logInfo_ "Listing Shifts." & runLog
        conn <- askConn
        Sqlite.Query.select_
          conn
          "select shift_id, nombre from shift",
      _listRoles = do
        logInfo_ "Listing Roles." & runLog
        conn <- askConn
        Sqlite.Query.select_
          conn
          "select role_id, shift_id, nombre from role",
      _listDaysOrdered = do
        logInfo_ "Listing Days." & runLog
        conn <- askConn
        Sqlite.Query.select_
          conn
          "select day_id, orden, nombre from day order by orden asc",
      _listJornadasByHorarioId = \horarioId -> do
        logInfo_ "Listing Jornadas by Horario ID." & runLog
        conn <- askConn
        Sqlite.Query.select
          conn
          "select day_id, gero_id, shift_id from jornada where horario_id = ?"
          [horarioId],
      _getHorarioMetaById = \horarioId -> do
        logInfo_ "Getting HorarioMeta by ID." & runLog
        conn <- askConn
        results <-
          Sqlite.Query.select
            conn
            "select horario_id, description, weekday_start, created_at from horario where horario_id = ?"
            [horarioId]
        pure $ listToMaybe results,
      _listHorariosMetaOrdered = do
        logInfo_ "Listing Horarios ordered by creation time." & runLog
        conn <- askConn
        Sqlite.Query.select_
          conn
          "select horario_id, description, weekday_start, created_at from horario order by created_at asc",
      _listAllExperimentsOrdered = do
        logInfo_ "Listing all Experiments ordered by creation time." & runLog
        conn <- askConn
        Sqlite.Query.select_
          conn
          "select experiment_id, description, horario_id, created_at, locked_at, selected_at from experiment order by created_at asc",
      _listExperimentsByHorarioIdOrdered = \horarioId -> do
        logInfo_ "Listing Experiments by Horario ID ordered by creation time." & runLog
        conn <- askConn
        Sqlite.Query.select
          conn
          "select experiment_id, description, horario_id, created_at, locked_at, selected_at from experiment where horario_id = ? order by created_at asc"
          [horarioId],
      _getExperimentById = \experimentId -> do
        logInfo_ "Getting experiment by ID." & runLog
        conn <- askConn
        results <-
          Sqlite.Query.select
            conn
            "select experiment_id, description, horario_id, created_at, locked_at, selected_at from experiment where experiment_id = ?"
            [experimentId]
        pure $ listToMaybe results,
      _listJornadasDetalladasByExperimentId = \experimentId -> do
        logInfo_ "Listing Jornadas Detalladas by Experiment." & runLog
        conn <- askConn
        Sqlite.Query.select
          conn
          "select experiment_id, day_id, gero_id, role_id from jornada_detallada where experiment_id = ?"
          [experimentId],
      _insertExperiment = \experiment -> do
        logInfo_ "Insert experiment" & runLog
        conn <- askConn
        Sqlite.Query.execute
          conn
          "insert into experiment (description, horario_id, created_at, locked_at, selected_at) values (?, ?, ?, ?, ?)"
          (experiment.description, experiment.horarioId, experiment.createdAt, experiment.lockedAt, experiment.selectedAt)
        experimentId <- ExperimentId <$> lastInsertRowId conn
        pure experiment {Experiment.experimentId},
      _updateExperiment = \experiment -> do
        logInfo_ "Update experiment" & runLog
        conn <- askConn
        Sqlite.Query.execute
          conn
          "update experiment set description = ?, horario_id = ?, created_at = ?, locked_at = ?, selected_at = ? where experiment_id = ?"
          (experiment.description, experiment.horarioId, experiment.createdAt, experiment.lockedAt, experiment.selectedAt, experiment.experimentId),
      _insertExperimentJornadasDetalladas = \jornadas -> do
        logInfo_ "Insert experiment jornadas" & runLog
        conn <- askConn
        Sqlite.Query.executeMany
          conn
          "insert into jornada_detallada (experiment_id, day_id, gero_id, role_id) values (?, ?, ?, ?)"
          jornadas,
      _permuteExperimentRow = \experimentId permutation -> do
        logInfo_ "Perform permutation" & runLog
        conn <- askConn
        -- Delete both rows involved in the swap
        Sqlite.Query.execute
          conn
          "DELETE FROM jornada_detallada WHERE experiment_id = ? AND ((day_id = ? AND role_id = ?) OR (day_id = ? AND role_id = ?))"
          (experimentId, permutation.day1, permutation.role1, permutation.day2, permutation.role2)
        -- Reinsert them with swapped gero assignments
        Sqlite.Query.executeMany
          conn
          "INSERT INTO jornada_detallada (experiment_id, day_id, gero_id, role_id) VALUES (?, ?, ?, ?)"
          [ (experimentId, permutation.day1, permutation.gero2, permutation.role1),
            (experimentId, permutation.day2, permutation.gero1, permutation.role2)
          ],
      _deleteExperiment = \experimentId -> do
        logInfo_ "Deleting experiment" & runLog
        conn <- askConn
        Sqlite.Query.execute
          conn
          "DELETE FROM jornada_detallada WHERE experiment_id = ?"
          [experimentId]
        Sqlite.Query.execute
          conn
          "DELETE FROM experiment WHERE experiment_id = ?"
          [experimentId]
    }
  where
    runLog = runLogT "sqliterepo" logger defaultLogLevel
