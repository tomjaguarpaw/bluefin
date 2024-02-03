module Main (main) where

import Bluefin.Internal
import Control.Monad (when)
import Data.Foldable (for_)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Prelude hiding (break, read)

main :: IO ()
main =
  if and
    [ oddsUntilFirstGreaterThan5 == [1, 3, 5, 7],
      [0, 1, 2, 3] !? 2 == Just 2,
      [0, 1, 2, 3] !? 4 == Nothing,
      runEff (handleException (eitherEff (Left True))) == (Left True :: Either Bool ()),
      runEff (handleException (eitherEff (Right True))) == (Right True :: Either () Bool)
    ]
    then pure ()
    else exitWith (ExitFailure 1)

(!?) :: [a] -> Int -> Maybe a
xs !? i = runEff $
  withEarlyReturn $ \ret -> do
    evalState 0 $ \s -> do
      for_ xs $ \a -> do
        i' <- read s
        when (i == i') (earlyReturn ret (Just a))
        write s (i' + 1)
    earlyReturn ret Nothing

oddsUntilFirstGreaterThan5 :: [Int]
oddsUntilFirstGreaterThan5 =
  fst $
    runEff $
      yieldToList $ \y -> do
        withJump $ \break -> do
          for_ [1 .. 10] $ \i -> do
            withJump $ \continue -> do
              when (i `mod` 2 == 0) $
                jumpTo continue
              yield y i
              when (i > 5) $
                jumpTo break

eitherEff :: e1 :> effs => Either e r -> Exception e e1 -> Eff effs r
eitherEff eith ex = case eith of
  Left e -> throw ex e
  Right r -> pure r
