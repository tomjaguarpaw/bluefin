module Bluefin.Examples.Random where

import Bluefin.IO (effIO, runEff_)
import Bluefin.Random (evalRandom)
import System.Random (initStdGen)
import System.Random.Stateful (randomRM)

exampleRandomUsage :: IO ()
exampleRandomUsage = runEff_ $ \io -> do
  g <- effIO io initStdGen
  evalRandom g $ \r -> do
    n <- randomRM @Int (1, 5) r
    m <- randomRM @Int (1, 5) r
    d <- randomRM @Double (0, 1) r
    effIO io $ print (n, m, d)
