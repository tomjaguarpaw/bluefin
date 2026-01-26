{-# LANGUAGE DerivingVia #-}

module Bluefin.Examples.DslBuilder.Robots where

import Bluefin.Compound
  ( Generic,
    Handle,
    OneWayCoercible (oneWayCoercibleImpl),
    OneWayCoercibleHandle (..),
    gOneWayCoercible,
    mapHandle,
  )
import Bluefin.DslBuilder (DslBuilder, dslBuilder, runDslBuilder)
import Bluefin.Eff (runPureEff, (:>))
import Bluefin.Stream (Stream, yield, yieldToList)
import Control.Monad (replicateM_, unless)
import Data.Foldable (for_)

data Arena = MkArena
  { arenaRobots :: [RobotEntry],
    arenaObstacles :: [ObstacleEntry]
  }
  deriving (Show, Eq)

type RobotEntry = (String, (Int, Int), Direction, [Instruction])

data Instruction = Wait Int | Forward | TurnLeft | TurnRight
  deriving (Show, Eq)

data Direction = N | E | S | W
  deriving (Show, Eq)

type ObstacleEntry = (Obstacle, (Int, Int))

data Obstacle = Sand | Rock | Iron
  deriving (Show, Eq)

-- > putStrLn (showArena myArena)
-- 5|
-- 4|  III
-- 3|  IbI
-- 2|  III
-- 1|
-- 0|r    S
--  +------
--   012345
myArena :: Arena
myArena =
  MkArena
    { arenaRobots =
        [ ("red", (0, 0), E, [Wait 100, Forward, Forward, Forward, Forward]),
          ("blue", (3, 3), N, [TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight, TurnRight])
        ],
      arenaObstacles = [(Sand, (5, 0)), (Iron, (2, 2)), (Iron, (2, 3)), (Iron, (2, 4)), (Iron, (3, 2)), (Iron, (3, 4)), (Iron, (4, 2)), (Iron, (4, 3)), (Iron, (4, 4))]
    }

-- > putStrLn (showArena myArena2)
-- 5|
-- 4|  III
-- 3|  IbI
-- 2|  III
-- 1|
-- 0|r    S
--  +------
--   012345
myArena2 :: Arena
myArena2 =
  MkArena
    { arenaRobots =
        [ ("red", (0, 0), E, Wait 100 : replicate 4 Forward),
          ("blue", (3, 3), N, replicate 20 TurnRight)
        ],
      arenaObstacles =
        (Sand, (5, 0))
          : [ (Iron, (x, y))
              | x <- [2 .. 4],
                y <- [2 .. 4],
                (x, y) /= (3, 3)
            ]
    }

myDslArena :: Arena
myDslArena = buildArena $ do
  -- 5|
  -- 4|
  -- 3|
  -- 2|
  -- 1|
  -- 0|R    S
  --  +------
  --   012345
  robot "red" (0, 0) E $ do
    wait 100
    forward 4

  obstacle Sand (5, 0)

  -- 5|
  -- 4|  III
  -- 3|  IBI
  -- 2|  III
  -- 1|
  -- 0|
  --  +------
  --   012345
  robot "blue" (3, 3) N $ do
    replicateM_ 10 aboutFace

  for_ [2 .. 4] $ \x -> do
    for_ [2 .. 4] $ \y -> do
      unless ((x, y) == (3, 3)) $ do
        obstacle Iron (x, y)

buildArena :: ArenaBuilder -> Arena
buildArena (MkArenaBuilder arenaBuilder) = runPureEff $ do
  (robots, obstacles) <- yieldToList $ \yrobots -> do
    (obstacles, ()) <- yieldToList $ \yobstacles -> do
      runDslBuilder
        (MkArenaH (mapHandle yrobots) (mapHandle yobstacles))
        arenaBuilder

    pure obstacles

  pure
    MkArena
      { arenaRobots = robots,
        arenaObstacles = obstacles
      }

test :: Bool
test = (myArena == myArena2) && (myArena == myDslArena)

newtype ArenaBuilder_ r
  = MkArenaBuilder (DslBuilder ArenaH r)
  deriving (Functor, Applicative, Monad)

type ArenaBuilder = ArenaBuilder_ ()

data ArenaH e = MkArenaH (Stream RobotEntry e) (Stream ObstacleEntry e)
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle ArenaH

instance (e :> es) => OneWayCoercible (ArenaH e) (ArenaH es) where
  oneWayCoercibleImpl = gOneWayCoercible

obstacle :: Obstacle -> (Int, Int) -> ArenaBuilder
obstacle strength coord =
  MkArenaBuilder $ dslBuilder $ \(MkArenaH _ yobstacle) -> do
    yield yobstacle (strength, coord)

robot ::
  String -> (Int, Int) -> Direction -> InstructionsBuilder -> ArenaBuilder
robot name coords dir (MkInstructionsBuilder ibuilder) =
  MkArenaBuilder $ dslBuilder $ \(MkArenaH yrobot _) -> do
    (insns, ()) <- yieldToList $ \yinsns -> do
      runDslBuilder (MkInstructionsH (mapHandle yinsns)) ibuilder

    yield yrobot (name, coords, dir, insns)

type InstructionsBuilder = InstructionsBuilder_ ()

newtype InstructionsBuilder_ r
  = MkInstructionsBuilder (DslBuilder InstructionsH r)
  deriving (Functor, Applicative, Monad)

data InstructionsH e = MkInstructionsH (Stream Instruction e)
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle InstructionsH

instance (e :> es) => OneWayCoercible (InstructionsH e) (InstructionsH es) where
  oneWayCoercibleImpl = gOneWayCoercible

instructionsBuilder :: Instruction -> InstructionsBuilder
instructionsBuilder insn =
  MkInstructionsBuilder $ dslBuilder $ \(MkInstructionsH yinsn) -> do
    yield yinsn insn

wait :: Int -> InstructionsBuilder
wait n = instructionsBuilder (Wait n)

turnLeft :: InstructionsBuilder
turnLeft = instructionsBuilder TurnLeft

turnRight :: InstructionsBuilder
turnRight = instructionsBuilder TurnRight

aboutFace :: InstructionsBuilder
aboutFace = do
  turnRight
  turnRight

forward :: Int -> InstructionsBuilder
forward n = replicateM_ n (instructionsBuilder Forward)

showArena :: Arena -> String
showArena arena = fst $ runPureEff $ yieldToList $ \ys -> do
  let headOrQ = \case
        [] -> '?'
        n : _ -> n

  (gridList, ()) <- yieldToList $ \ygrid -> do
    for_ (arenaRobots arena) $ \(name, coord, _, _) -> do
      yield ygrid (coord, headOrQ name)

    for_ (arenaObstacles arena) $ \(obs, coord) -> do
      yield ygrid (coord, headOrQ (show obs))

  -- In a real world implementation we'd gather `gridList` into a
  -- `Map` to check for clashing keys and to get sublinear lookups.
  -- For this example we'll just look up grid coordinates in the list
  -- itself.

  for_ [5, 4 .. 0] $ \y -> do
    yield ys (headOrQ (reverse (show y)))
    yield ys '|'

    for_ [0 .. 5] $ \x -> do
      let c = case lookup (x, y) gridList of
            Nothing -> ' '
            Just c' -> c'

      yield ys c

    yield ys '\n'

  for_ " +" (yield ys)

  replicateM_ (5 + 1) (yield ys '-')

  yield ys '\n'

  for_ "  " (yield ys)

  for_ [0 .. 5] $ \x -> do
    yield ys (headOrQ (reverse (show @Int x)))
