{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Bornet.Model
  ( GeroId (..),
    dummyGeroId,
    GroupId (..),
    Gero (..),
    Geros,
    makeGeros,
    ShiftId (..),
    makeShifts,
    dummyShiftId,
    morningShift,
    afternoonShift,
    Shift (..),
    DayShifts,
    DayId (..),
    dummyDayId,
    Day (..),
    Days,
    makeDays,
    Jornada (..),
    HorarioId (..),
    dummyHorarioId,
    HorarioDescription (..),
    WeekdayName (..),
    weekdayNameToText,
    HorarioMeta (..),
    ExperimentId (..),
    dummyExperimentId,
    ExperimentDescription (..),
    Experiment (..),
    RoleId (..),
    dummyRoleId,
    Role (..),
    Roles,
    JornadaDetallada (..),
    Horario_ (..),
    Horario,
    HorarioDetallado (..),
    DetailedDayShifts,
    makeHorarioDetallado0,
    makeRoles,
    makeRolesByShift,
    horarioDetalladoToJornadasDetalladas,
    Permutation (..),
    GeroSwap (..),
    UnixEpoch (..),
    currentUnixEpoch,
    HorarioDetalladoAnalysis,
    analyzeHorarioDetallado,
    forDaysOrdered,
    decorateWithWeekdayNames,
    ExperimentImitation (..),
    generateImitationPermutations,
    isValidPermutation,
  )
where

import Data.Aeson as Aeson
import Data.Foldable qualified
import Data.Function ((&))
import Data.Int
import Data.List
import Data.List qualified
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Typeable
import GHC.Generics (Generic)
import Bornet.Prelude
import Lucid (ToHtml)
import Optics.Dot
import Servant (FromHttpApiData, ToHttpApiData)
import Sqlite qualified
import Sqlite.Query.FromField
import Sqlite.Query.FromRow
import Sqlite.Query.Ok
import Sqlite.Query.ToField
import Sqlite.Query.ToRow
import Web.FormUrlEncoded

newtype GeroId = GeroId Int64
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromField, ToField, ToJSON, FromHttpApiData, ToHttpApiData)

newtype GroupId = GroupId Int64
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromField, ToField)

data Gero = Gero
  { geroId :: GeroId,
    nombre :: Text,
    groupId :: GroupId
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)

type Geros = Map GeroId Gero

newtype ShiftId = ShiftId Int64
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromField, ToField)

data Shift = Shift
  { shiftId :: ShiftId,
    nombre :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)

newtype DayId = DayId Int64
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromField, ToField, ToJSON, FromHttpApiData, ToHttpApiData)

data Day = Day
  { dayId :: DayId,
    orden :: Int64,
    nombre :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)

type Days = Map DayId (Day, WeekdayName)

makeDays :: List (Day, WeekdayName) -> Map DayId (Day, WeekdayName)
makeDays dayList = Map.fromList do
  d@(day, _) <- dayList
  [(day.dayId, d)]

data Jornada = Jornada
  { dayId :: DayId,
    geroId :: GeroId,
    shiftId :: ShiftId
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)

newtype HorarioId = HorarioId Int64
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromField, ToField, FromHttpApiData, ToHttpApiData)

newtype HorarioDescription = HorarioDescription {description :: Text}
  deriving newtype (Eq, Ord, Show, FromField, ToField, ToHtml)
  deriving stock (Generic)
  deriving anyclass (FromForm, ToForm)

newtype WeekdayName = WeekdayName Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromField, ToField)

weekdayNameToText :: WeekdayName -> Text
weekdayNameToText (WeekdayName t) = t

data HorarioMeta = HorarioMeta
  { horarioId :: HorarioId,
    description :: HorarioDescription,
    weekdayStart :: WeekdayName,
    createdAt :: UnixEpoch
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (DotOptics) via GenericFields (HorarioMeta)

newtype ExperimentId = ExperimentId Int64
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromField, ToField, FromHttpApiData, ToHttpApiData)

newtype ExperimentDescription = ExperimentDescription {description :: Text}
  deriving newtype (Eq, Ord, Show, FromField, ToField, ToHtml)
  deriving stock (Generic)
  -- Record fields are used for form field names
  deriving anyclass (FromForm, ToForm)

data Experiment = Experiment
  { experimentId :: ExperimentId,
    description :: ExperimentDescription,
    horarioId :: HorarioId,
    createdAt :: UnixEpoch,
    lockedAt :: Maybe UnixEpoch,
    selectedAt :: Maybe UnixEpoch
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (DotOptics) via GenericFields (Experiment)

newtype RoleId = RoleId Int64
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, FromField, ToField, ToJSON, FromHttpApiData)

data Role = Role
  { roleId :: RoleId,
    shiftId :: ShiftId,
    nombre :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)

data JornadaDetallada = JornadaDetallada
  { experimentId :: ExperimentId,
    dayId :: DayId,
    geroId :: GeroId,
    roleId :: RoleId
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)

data Horario_ f = Horario
  { meta :: HorarioMeta,
    days :: Days,
    daysOrdered :: List (Day, WeekdayName),
    geros :: Geros,
    shifts :: Shifts,
    horario :: Map DayId (Map ShiftId (f GeroId))
  }
  deriving stock (Generic)
  deriving (DotOptics) via GenericFields (Horario_ f)

data HorarioDetallado = HorarioDetallado
  { experiment :: Experiment,
    roles :: Roles,
    rolesByShift :: RolesByShift,
    horarioDetallado :: Horario_ (Map RoleId)
  }
  deriving stock (Generic)
  deriving (DotOptics) via GenericFields (HorarioDetallado)

type Horario = Horario_ Set

type Shifts = Map ShiftId Shift

type DayShifts = Map ShiftId (Set GeroId)

type DetailedDayShifts = Map ShiftId (Map RoleId GeroId)

type HorarioDetalladoAnalysis = Map GeroId (Map RoleId Int)

analyzeHorarioDetallado :: HorarioDetallado -> HorarioDetalladoAnalysis
analyzeHorarioDetallado horarioDetallado =
  Map.fromListWith (Map.unionWith (+)) $ do
    (_dayId, shifts) <- Map.toList horarioDetallado.horarioDetallado.horario
    (_shiftId, rolesMap) <- Map.toList shifts
    (roleId, geroId) <- Map.toList rolesMap
    return (geroId, Map.singleton roleId 1)

type RolesByShift = Map ShiftId Roles

makeRolesByShift :: (Foldable f) => f Role -> RolesByShift
makeRolesByShift roles =
  Map.fromListWith Map.union $ do
    rol <- Data.Foldable.toList roles
    return (rol.shiftId, Map.singleton rol.roleId rol)

-- | Assumes that the number of geros always matches the number of roles.
makeHorarioDetallado0 :: Experiment -> Roles -> Horario -> HorarioDetallado
makeHorarioDetallado0 experiment roles horario =
  let rolesByShift = makeRolesByShift roles
      gerosToRoles shiftId geroIds =
        let roleIds = rolesByShift & xlookupS "makeHorarioDetallado0: finding roles for shift" shiftId & Map.keysSet
         in Map.fromList (Data.List.zip (Set.toList roleIds) (Set.toList geroIds))
   in HorarioDetallado
        { experiment,
          roles,
          rolesByShift,
          horarioDetallado =
            -- Type-changing update
            horario {horario = Map.map (Map.mapWithKey gerosToRoles) horario.horario}
        }

type Roles = Map RoleId Role

makeRoles :: [Role] -> Roles
makeRoles roles = Map.fromList do
  rol <- roles
  [(rol.roleId, rol)]

-- | Inverse operation to makeHorarioDetallado
horarioDetalladoToJornadasDetalladas :: ExperimentId -> HorarioDetallado -> [JornadaDetallada]
horarioDetalladoToJornadasDetalladas experimentId horario = do
  (dayId, shifts) <- Map.toList horario.horarioDetallado.horario
  (_shiftId, rolesMap) <- Map.toList shifts
  (roleId, geroId) <- Map.toList rolesMap
  return
    JornadaDetallada
      { experimentId = experimentId,
        dayId = dayId,
        geroId = geroId,
        roleId = roleId
      }

makeGeros :: [Gero] -> Map GeroId Gero
makeGeros geros = Map.fromList do
  gero <- geros
  [(gero.geroId, gero)]

makeShifts :: [Shift] -> Map ShiftId Shift
makeShifts shifts = Map.fromList do
  shift <- shifts
  [(shift.shiftId, shift)]

morningShift :: ShiftId
morningShift = ShiftId 1

afternoonShift :: ShiftId
afternoonShift = ShiftId 2

dummyGeroId :: GeroId
dummyGeroId = GeroId 0

dummyShiftId :: ShiftId
dummyShiftId = ShiftId 0

dummyDayId :: DayId
dummyDayId = DayId 0

dummyHorarioId :: HorarioId
dummyHorarioId = HorarioId 0

dummyExperimentId :: ExperimentId
dummyExperimentId = ExperimentId 0

dummyRoleId :: RoleId
dummyRoleId = RoleId 0

data Permutation = Permutation
  { day1 :: DayId,
    role1 :: RoleId,
    gero1 :: GeroId,
    day2 :: DayId,
    role2 :: RoleId,
    gero2 :: GeroId
  }
  deriving stock (Show, Generic, Ord, Eq)
  deriving anyclass (FromForm)

-- | A simpler swap request for permuting all roles between two geros
-- for all the days in a given experiment.
data GeroSwap = GeroSwap
  { gero1 :: GeroId,
    gero2 :: GeroId
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)

newtype UnixEpoch = UnixEpoch Int64
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToField, FromField)

secondsSinceEpoch :: UTCTime -> Int64
secondsSinceEpoch =
  floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

currentUnixEpoch :: IO UnixEpoch
currentUnixEpoch = do
  UnixEpoch . secondsSinceEpoch <$> getCurrentTime

weekdayNames :: [WeekdayName]
weekdayNames = [WeekdayName "L", WeekdayName "M", WeekdayName "X", WeekdayName "J", WeekdayName "V", WeekdayName "S", WeekdayName "D"]

forDaysOrdered :: (Applicative m) => (Day -> WeekdayName -> m ()) -> Horario_ f -> m ()
forDaysOrdered f Horario {daysOrdered} =
  Data.Foldable.for_ daysOrdered (uncurry f)

decorateWithWeekdayNames :: WeekdayName -> List Day -> List (Day, WeekdayName)
decorateWithWeekdayNames firstWeekdayName days =
  let cyclingWeekdayNames = take 100 (cycle weekdayNames) ++ repeat (WeekdayName "Z")
   in zip days (dropWhile (/= firstWeekdayName) cyclingWeekdayNames)

data ExperimentImitation e = MakeExperimentImitation
  { source :: e,
    sourceStartDay :: DayId,
    sourceEndDay :: DayId,
    targetStartDay :: DayId
  }
  deriving stock (Eq, Ord, Show, Generic)

deriving instance (FromHttpApiData e) => FromForm (ExperimentImitation e)

deriving instance (ToHttpApiData e) => ToForm (ExperimentImitation e)

-- | Given a source @ExperimentImitation HorarioDetallado@ and a target @HorarioDetallado@,
-- find the days that overlap between the two (start and end day in source, begin day in target)
-- and then generate a list of @Permutation@ that would make the target days more "similar" to
-- the source days.
--
-- The achieved similarity need not be perfect, but must be valid. Like it
-- happens with the individual "role permutations", we can only exchange roles
-- between geros in the same shift in the same day. Permuting between morning
-- and afternoon is forbidden!
--
-- This function exists to replace a tiresome manual process of making a new
-- experiment roughly similar to an existing one (in days with similar
-- structures).
generateImitationPermutations :: ExperimentImitation HorarioDetallado -> HorarioDetallado -> [Permutation]
generateImitationPermutations imitation target =
  let sourceHorario = imitation.source.horarioDetallado
      targetHorario = target.horarioDetallado
      -- Extract source days slice (from sourceStartDay to sourceEndDay inclusive)
      sourceDays =
        takeWhileInclusive (\(day, _) -> day.dayId /= imitation.sourceEndDay)
          . dropWhile (\(day, _) -> day.dayId /= imitation.sourceStartDay)
          $ sourceHorario.daysOrdered
      -- Extract target days starting from targetStartDay, same count
      targetDays =
        take (length sourceDays)
          . dropWhile (\(day, _) -> day.dayId /= imitation.targetStartDay)
          $ targetHorario.daysOrdered
      -- Pair source and target day IDs
      dayPairs = zip (map ((.dayId) . fst) sourceDays) (map ((.dayId) . fst) targetDays)
      targetGeros = targetHorario.geros
      targetRoles = target.roles
   in concatMap (generateForDayPair sourceHorario targetHorario targetGeros targetRoles) dayPairs
  where
    takeWhileInclusive _ [] = []
    takeWhileInclusive p (x : xs) = x : if p x then takeWhileInclusive p xs else []

    generateForDayPair :: Horario_ (Map RoleId) -> Horario_ (Map RoleId) -> Geros -> Roles -> (DayId, DayId) -> [Permutation]
    generateForDayPair sourceH targetH geros roles (srcDayId, tgtDayId) =
      let srcDayShifts = Map.findWithDefault Map.empty srcDayId sourceH.horario
          tgtDayShifts = Map.findWithDefault Map.empty tgtDayId targetH.horario
       in concatMap
            ( \shiftId ->
                case (Map.lookup shiftId srcDayShifts, Map.lookup shiftId tgtDayShifts) of
                  (Just srcRoles, Just tgtRoles) -> matchShift geros roles tgtDayId srcRoles tgtRoles
                  _ -> []
            )
            (Map.keys srcDayShifts)

    -- Greedily swap geros in the target to match the source assignment.
    -- For each role in the source, if the target has a different gero, find where
    -- the desired gero is in the target and generate a swap.
    -- Only generates a permutation when both geros share the same groupId
    -- and both roles correspond to the same shift.
    matchShift :: Geros -> Roles -> DayId -> Map RoleId GeroId -> Map RoleId GeroId -> [Permutation]
    matchShift _geros roles targetDayId sourceAssignment = go (Map.toList sourceAssignment)
      where
        sameShift r1 r2 =
          case (Map.lookup r1 roles, Map.lookup r2 roles) of
            (Just role1, Just role2) -> role1.shiftId == role2.shiftId
            _ -> False
        go :: [(RoleId, GeroId)] -> Map RoleId GeroId -> [Permutation]
        go [] _ = []
        go ((roleId, desiredGero) : rest) currentTarget =
          -- what gero fills the source role in the target?
          case Map.lookup roleId currentTarget of
            -- No gero assigned to the role in the target? skip.
            Nothing -> go rest currentTarget
            -- Ok, this is the gero that does the role in the target.
            Just currentGero
              | currentGero == desiredGero -> go rest currentTarget
              | otherwise ->
                  let gero2role = Map.fromList [(g, r) | (r, g) <- Map.toList currentTarget]
                   in -- What role does the source gero in the target?
                      case Map.lookup desiredGero gero2role of
                        -- The source gero does nothing in the target? Skip.
                        Nothing -> go rest currentTarget
                        Just otherRole
                          -- Skip if roles are from different shifts
                          -- \| not (sameGroup currentGero desiredGero) -> go rest currentTarget
                          | not (sameShift roleId otherRole) -> go rest currentTarget
                          | otherwise ->
                              let perm =
                                    Permutation
                                      { day1 = targetDayId,
                                        role1 = roleId,
                                        gero1 = currentGero,
                                        day2 = targetDayId,
                                        role2 = otherRole,
                                        gero2 = desiredGero
                                      }
                                  -- Here what we are basically doing is to apply a perm locally.
                                  -- Maybe abstract it into its own function.
                                  newTarget = Map.insert roleId desiredGero $ Map.insert otherRole currentGero currentTarget
                               in perm : go rest newTarget

isValidPermutation :: Roles -> Permutation -> Bool
isValidPermutation roles permutation =
  let role1 = roles & xlookupS "permute: looking up role1" permutation.role1
      role2 = roles & xlookupS "permute: looking up role2" permutation.role2
   in permutation.day1 == permutation.day2 && role1.shiftId == role2.shiftId
