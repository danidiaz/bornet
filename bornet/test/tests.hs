{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.Foldable qualified
import Data.Function ((&))
import Data.IORef
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid
import Data.Proxy
import Data.Set qualified
import Data.Typeable (typeRep)
import Bornet.Model
import Optics.Core
import Optics.Dot
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "same" do
        let same =
              generateImitationPermutations
                ( MakeExperimentImitation
                    { source = dummyHorarioDetallado,
                      sourceStartDay = DayId 1,
                      sourceEndDay = DayId 5,
                      targetStartDay = DayId 1
                    }
                )
                dummyHorarioDetallado
        assertEqual "when there are no changes" [] same
        pure (),
      testCase "one" do
        let same =
              generateImitationPermutations
                ( MakeExperimentImitation
                    { source = dummyHorarioDetallado',
                      sourceStartDay = DayId 1,
                      sourceEndDay = DayId 5,
                      targetStartDay = DayId 1
                    }
                )
                dummyHorarioDetallado
        assertEqual "when there are no changes" [Permutation {day1 = DayId 1, role1 = RoleId 1, gero1 = GeroId 1, day2 = DayId 1, role2 = RoleId 2, gero2 = GeroId 2}] same,
      testCase "one not included" do
        let same =
              generateImitationPermutations
                ( MakeExperimentImitation
                    { source = dummyHorarioDetallado',
                      sourceStartDay = DayId 2,
                      sourceEndDay = DayId 5,
                      targetStartDay = DayId 2
                    }
                )
                dummyHorarioDetallado
        assertEqual "when there are no changes" [] same,
      testCase "one no mismo grupo" do
        let same =
              generateImitationPermutations
                ( MakeExperimentImitation
                    { source = dummyHorarioDetallado',
                      sourceStartDay = DayId 1,
                      sourceEndDay = DayId 5,
                      targetStartDay = DayId 1
                    }
                )
                dummyHorarioDetalladoNoMismoGrupo
        assertEqual "when there are no changes" [Permutation {day1 = DayId 1, role1 = RoleId 1, gero1 = GeroId 5, day2 = DayId 1, role2 = RoleId 2, gero2 = GeroId 2}] same,
      testCase "one no mismo grupo algunos sí" do
        let same =
              generateImitationPermutations
                ( MakeExperimentImitation
                    { source = dummyHorarioDetallado',
                      sourceStartDay = DayId 1,
                      sourceEndDay = DayId 5,
                      targetStartDay = DayId 1
                    }
                )
                dummyHorarioDetalladoNoMismoGrupoAlgunosSi
        assertEqual "when there are no changes" [Permutation {day1 = DayId 1, role1 = RoleId 1, gero1 = GeroId 5, day2 = DayId 1, role2 = RoleId 2, gero2 = GeroId 2}, Permutation {day1 = DayId 2, role1 = RoleId 1, gero1 = GeroId 2, day2 = DayId 2, role2 = RoleId 2, gero2 = GeroId 1}] same,
      testCase "two no mismo grupo algunos sí 2" do
        let same =
              generateImitationPermutations
                ( MakeExperimentImitation
                    { source = dummyHorarioDetallado',
                      sourceStartDay = DayId 1,
                      sourceEndDay = DayId 5,
                      targetStartDay = DayId 1
                    }
                )
                dummyHorarioDetalladoNoMismoGrupoAlgunosSi2
        assertEqual "when there are no changes" [Permutation {day1 = DayId 1, role1 = RoleId 1, gero1 = GeroId 5, day2 = DayId 1, role2 = RoleId 2, gero2 = GeroId 2}, Permutation {day1 = DayId 2, role1 = RoleId 1, gero1 = GeroId 2, day2 = DayId 2, role2 = RoleId 2, gero2 = GeroId 1}, Permutation {day1 = DayId 2, role1 = RoleId 3, gero1 = GeroId 4, day2 = DayId 2, role2 = RoleId 4, gero2 = GeroId 3}] same
        pure (),
      testCase "two no mismo grupo algunos sí 2 diff days" do
        let same =
              generateImitationPermutations
                ( MakeExperimentImitation
                    { source = dummyHorarioDetallado',
                      sourceStartDay = DayId 1,
                      sourceEndDay = DayId 5,
                      targetStartDay = DayId 1
                    }
                )
                dummyHorarioDetalladoNoMismoGrupoAlgunosSi2'
        assertEqual "when there are no changes" [Permutation {day1 = DayId 1, role1 = RoleId 1, gero1 = GeroId 5, day2 = DayId 1, role2 = RoleId 2, gero2 = GeroId 2}, Permutation {day1 = DayId 2, role1 = RoleId 1, gero1 = GeroId 2, day2 = DayId 2, role2 = RoleId 2, gero2 = GeroId 1}, Permutation {day1 = DayId 3, role1 = RoleId 3, gero1 = GeroId 4, day2 = DayId 3, role2 = RoleId 4, gero2 = GeroId 3}] same
        pure ()
    ]

dummyShifts :: Map ShiftId Shift
dummyShifts =
  makeShifts
    [ Shift {shiftId = morningShift, nombre = "Mañana"},
      Shift {shiftId = afternoonShift, nombre = "Tarde"}
    ]

dummyRoleList :: [Role]
dummyRoleList =
  [ Role {roleId = RoleId 1, shiftId = morningShift, nombre = "M1"},
    Role {roleId = RoleId 2, shiftId = morningShift, nombre = "M2"},
    Role {roleId = RoleId 3, shiftId = morningShift, nombre = "M3"},
    Role {roleId = RoleId 4, shiftId = morningShift, nombre = "M4"},
    Role {roleId = RoleId 5, shiftId = afternoonShift, nombre = "T1"},
    Role {roleId = RoleId 6, shiftId = afternoonShift, nombre = "T2"}
  ]

dummyRoles :: Roles
dummyRoles = makeRoles dummyRoleList

dummyGerosList :: [Gero]
dummyGerosList =
  [ Gero {geroId = GeroId 1, nombre = "Ana", groupId = GroupId 1},
    Gero {geroId = GeroId 2, nombre = "Bea", groupId = GroupId 1},
    Gero {geroId = GeroId 3, nombre = "Carlos", groupId = GroupId 1},
    Gero {geroId = GeroId 4, nombre = "Diana", groupId = GroupId 1},
    Gero {geroId = GeroId 5, nombre = "Gus", groupId = GroupId 2},
    Gero {geroId = GeroId 6, nombre = "John", groupId = GroupId 2},
    Gero {geroId = GeroId 7, nombre = "Jenny", groupId = GroupId 2},
    Gero {geroId = GeroId 8, nombre = "Lara", groupId = GroupId 2}
  ]

dummyDaysList :: [Day]
dummyDaysList =
  [ Day {dayId = DayId 1, orden = 1, nombre = "Dia 1"},
    Day {dayId = DayId 2, orden = 2, nombre = "Dia 2"},
    Day {dayId = DayId 3, orden = 3, nombre = "Dia 3"},
    Day {dayId = DayId 4, orden = 4, nombre = "Dia 4"},
    Day {dayId = DayId 5, orden = 5, nombre = "Dia 5"}
  ]

dummyDaysOrdered :: [(Day, WeekdayName)]
dummyDaysOrdered = decorateWithWeekdayNames (WeekdayName "L") dummyDaysList

dummyExperiment :: Experiment
dummyExperiment =
  Experiment
    { experimentId = ExperimentId 1,
      description = ExperimentDescription "Test experiment",
      horarioId = HorarioId 1,
      createdAt = UnixEpoch 1000000,
      lockedAt = Nothing,
      selectedAt = Nothing
    }

-- Each day: morning has gero 1 -> role 1, gero 2 -> role 2
--           afternoon has gero 3 -> role 3, gero 4 -> role 4
dummyDayAssignment :: Map ShiftId (Map RoleId GeroId)
dummyDayAssignment =
  Map.fromList
    [ ( morningShift,
        Map.fromList
          [ (RoleId 1, GeroId 1),
            (RoleId 2, GeroId 2),
            (RoleId 3, GeroId 3),
            (RoleId 4, GeroId 4)
          ]
      ),
      ( afternoonShift,
        Map.fromList
          [ (RoleId 5, GeroId 5),
            (RoleId 6, GeroId 6)
          ]
      )
    ]

dummyHorarioDetallado :: HorarioDetallado
dummyHorarioDetallado =
  let horarioBase =
        Horario
          { meta =
              HorarioMeta
                { horarioId = HorarioId 1,
                  description = HorarioDescription "Test horario",
                  weekdayStart = WeekdayName "L",
                  createdAt = UnixEpoch 1000000
                },
            days = makeDays dummyDaysOrdered,
            daysOrdered = dummyDaysOrdered,
            geros = makeGeros dummyGerosList,
            shifts = dummyShifts,
            horario =
              Map.fromList
                [(DayId d, dummyDayAssignment) | d <- [1 .. 5]]
          }
   in HorarioDetallado
        { experiment = dummyExperiment,
          roles = dummyRoles,
          rolesByShift = makeRolesByShift dummyRoleList,
          horarioDetallado = horarioBase
        }

dummyHorarioDetallado' :: HorarioDetallado
dummyHorarioDetallado' =
  dummyHorarioDetallado
    & (the.horarioDetallado.horario % at (DayId 1) % _Just % at morningShift % _Just % at (RoleId 1) .~ Just (GeroId 2))
    & (the.horarioDetallado.horario % at (DayId 1) % _Just % at morningShift % _Just % at (RoleId 2) .~ Just (GeroId 1))

dummyHorarioDetalladoNoMismoGrupo :: HorarioDetallado
dummyHorarioDetalladoNoMismoGrupo =
  dummyHorarioDetallado
    & (the.horarioDetallado.horario % at (DayId 1) % _Just % at morningShift % _Just % at (RoleId 1) .~ Just (GeroId 5))
    & (the.horarioDetallado.horario % at (DayId 1) % _Just % at afternoonShift % _Just % at (RoleId 5) .~ Just (GeroId 1))

dummyHorarioDetalladoNoMismoGrupoAlgunosSi :: HorarioDetallado
dummyHorarioDetalladoNoMismoGrupoAlgunosSi =
  dummyHorarioDetallado
    & (the.horarioDetallado.horario % at (DayId 1) % _Just % at morningShift % _Just % at (RoleId 1) .~ Just (GeroId 5))
    & (the.horarioDetallado.horario % at (DayId 1) % _Just % at afternoonShift % _Just % at (RoleId 5) .~ Just (GeroId 1))
    & (the.horarioDetallado.horario % at (DayId 2) % _Just % at morningShift % _Just % at (RoleId 1) .~ Just (GeroId 2))
    & (the.horarioDetallado.horario % at (DayId 2) % _Just % at morningShift % _Just % at (RoleId 2) .~ Just (GeroId 1))

dummyHorarioDetalladoNoMismoGrupoAlgunosSi2 :: HorarioDetallado
dummyHorarioDetalladoNoMismoGrupoAlgunosSi2 =
  dummyHorarioDetallado
    & (the.horarioDetallado.horario % at (DayId 1) % _Just % at morningShift % _Just % at (RoleId 1) .~ Just (GeroId 5))
    & (the.horarioDetallado.horario % at (DayId 1) % _Just % at afternoonShift % _Just % at (RoleId 5) .~ Just (GeroId 1))
    & (the.horarioDetallado.horario % at (DayId 2) % _Just % at morningShift % _Just % at (RoleId 1) .~ Just (GeroId 2))
    & (the.horarioDetallado.horario % at (DayId 2) % _Just % at morningShift % _Just % at (RoleId 2) .~ Just (GeroId 1))
    & (the.horarioDetallado.horario % at (DayId 2) % _Just % at morningShift % _Just % at (RoleId 3) .~ Just (GeroId 4))
    & (the.horarioDetallado.horario % at (DayId 2) % _Just % at morningShift % _Just % at (RoleId 4) .~ Just (GeroId 3))

dummyHorarioDetalladoNoMismoGrupoAlgunosSi2' :: HorarioDetallado
dummyHorarioDetalladoNoMismoGrupoAlgunosSi2' =
  dummyHorarioDetallado
    & (the.horarioDetallado.horario % at (DayId 1) % _Just % at morningShift % _Just % at (RoleId 1) .~ Just (GeroId 5))
    & (the.horarioDetallado.horario % at (DayId 1) % _Just % at afternoonShift % _Just % at (RoleId 5) .~ Just (GeroId 1))
    & (the.horarioDetallado.horario % at (DayId 2) % _Just % at morningShift % _Just % at (RoleId 1) .~ Just (GeroId 2))
    & (the.horarioDetallado.horario % at (DayId 2) % _Just % at morningShift % _Just % at (RoleId 2) .~ Just (GeroId 1))
    & (the.horarioDetallado.horario % at (DayId 3) % _Just % at morningShift % _Just % at (RoleId 3) .~ Just (GeroId 4))
    & (the.horarioDetallado.horario % at (DayId 3) % _Just % at morningShift % _Just % at (RoleId 4) .~ Just (GeroId 3))
