module Main (main) where

import Bluefin.Internal
import Control.Monad (when)
import Data.Foldable (for_)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Prelude hiding (break)

main :: IO ()
main =
  if oddsUntilFirstGreaterThan5 == [1, 3, 5, 7]
    then pure ()
    else exitWith (ExitFailure 1)

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
