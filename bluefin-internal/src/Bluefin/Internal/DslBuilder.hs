module Bluefin.Internal.DslBuilder where

import Bluefin.Internal
import Control.Monad (replicateM_, unless)
import Data.Foldable (for_)

newtype DslBuilder h r
  = MkDslBuilder {unMkDslBuilder :: forall e. h e -> Eff e r}

runDslBuilder :: h e -> DslBuilder h r -> Eff e r
runDslBuilder h f = unMkDslBuilder f h

dslBuilder :: (forall e. h e -> Eff e r) -> DslBuilder h r
dslBuilder = MkDslBuilder

instance Functor (DslBuilder h) where
  fmap f g = dslBuilder ((fmap . fmap) f (unMkDslBuilder g))

instance Applicative (DslBuilder h) where
  pure x = dslBuilder (pure (pure x))
  f <*> x = dslBuilder (\h -> runDslBuilder h f <*> runDslBuilder h x)

instance Monad (DslBuilder h) where
  m >>= f = dslBuilder $ \h -> do
    r <- runDslBuilder h m
    runDslBuilder h (f r)

-- 5|
-- 4|
-- 3|
-- 2|
-- 1|
-- 0|R    S
--  +------
--   012345

-- 5|
-- 4|  III
-- 3|  IBI
-- 2|  III
-- 1|
-- 0|
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

test :: Bool
test = (myArena == myArena2) && (myArena == myDslArena)

data Instruction = Wait Int | Forward | TurnLeft | TurnRight
  deriving (Show, Eq)

data Direction = N | E | S | W
  deriving (Show, Eq)

type Robot = (String, (Int, Int), Direction, [Instruction])

type Obstacle = (Strength, (Int, Int))

data InstructionsH e = MkInstructionsH !(Stream Instruction e)

instance Handle InstructionsH where
  mapHandle (MkInstructionsH s) = MkInstructionsH (mapHandle s)

data ArenaH e = MkArenaH !(Stream Robot e) !(Stream Obstacle e)

data Arena = MkArena
  { arenaRobots :: [Robot],
    arenaObstacles :: [Obstacle]
  }
  deriving (Show, Eq)

newtype InstructionsBuilder_ r
  = MkInstructionsBuilder (DslBuilder InstructionsH r)
  deriving (Functor, Applicative, Monad)

newtype ArenaBuilder_ r
  = MkArenaBuilder (DslBuilder ArenaH r)
  deriving (Functor, Applicative, Monad)

type InstructionsBuilder = InstructionsBuilder_ ()

type ArenaBuilder = ArenaBuilder_ ()

robot ::
  String -> (Int, Int) -> Direction -> InstructionsBuilder -> ArenaBuilder
robot name coords dir (MkInstructionsBuilder ibuilder) =
  MkArenaBuilder $ dslBuilder $ \(MkArenaH yrobot _) -> do
    (insns, ()) <- yieldToList $ \yinsns -> do
      runDslBuilder (MkInstructionsH (mapHandle yinsns)) ibuilder

    yield yrobot (name, coords, dir, insns)

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

data Strength = Sand | Rock | Iron
  deriving (Show, Eq)

obstacle :: Strength -> (Int, Int) -> ArenaBuilder
obstacle strength coord =
  MkArenaBuilder $ dslBuilder $ \(MkArenaH _ yobstacle) -> do
    yield yobstacle (strength, coord)

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
