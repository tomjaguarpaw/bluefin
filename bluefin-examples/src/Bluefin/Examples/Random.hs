module Bluefin.Examples.Random where

import Bluefin.IO (effIO, runEff_)
import Bluefin.Random (evalRandom, withInitStdGen)
import System.Random.Stateful (randomRM)

exampleRandomUsage :: IO ()
exampleRandomUsage = runEff_ $ \io -> do
  withInitStdGen io $ \r -> do
    n <- randomRM @Int (1, 5) r
    m <- randomRM @Int (1, 5) r
    d <- randomRM @Double (0, 1) r
    effIO io $ print (n, m, d)
