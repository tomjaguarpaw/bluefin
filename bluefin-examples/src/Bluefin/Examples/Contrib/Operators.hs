module Bluefin.Examples.Contrib.Operators where

import Bluefin.Contrib.Operators ((+=))
import Bluefin.Eff (runEff_)
import Bluefin.IO (effIO)
import Bluefin.State (evalState, get)
import Data.Foldable (for_)

example :: IO ()
example = runEff_ $ \io -> evalState @Int 0 $ \st -> do
  for_ [1 .. 5] $ \i -> do
    n <- get st
    let msg = "Triangular number " <> show i <> " is " <> show n
    effIO io (putStrLn msg)
    st += i
