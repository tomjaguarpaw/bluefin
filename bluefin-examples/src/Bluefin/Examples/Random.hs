module Bluefin.Examples.Random where

import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (effIO, runEff_)
import Bluefin.Random (Random, withInitStdGen)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import System.Random.Stateful (RandomGen, Uniform, UniformRange, randomRM)
import qualified System.Random.Stateful as RS

exampleRandomUsage :: IO ()
exampleRandomUsage = runEff_ $ \io -> do
  withInitStdGen io $ \r -> do
    n <- randomRM @Int (1, 5) r
    m <- randomRM @Int (1, 5) r
    d <- randomRM @Double (0, 1) r
    effIO io $ print (n, m, d)

uniformM ::
  (Uniform a, RandomGen g, e1 :> es) =>
  Random g e1 ->
  Eff es a
uniformM = RS.uniformM

uniformRM ::
  (UniformRange a, RandomGen g, e1 :> es) =>
  (a, a) ->
  Random g e1 ->
  Eff es a
uniformRM = RS.uniformRM

uniformListM ::
  (Uniform a, RandomGen g, e1 :> es) =>
  Int ->
  Random g e1 ->
  Eff es [a]
uniformListM = RS.uniformListM

uniformListRM ::
  (UniformRange a, RandomGen g, e1 :> es) =>
  Int ->
  (a, a) ->
  Random g e1 ->
  Eff es [a]
uniformListRM = RS.uniformListRM

uniformShuffleListM ::
  (RandomGen g, e1 :> es) =>
  [a] ->
  Random g e1 ->
  Eff es [a]
uniformShuffleListM = RS.uniformShuffleListM

{-
uniformByteArrayM ::
  (RandomGen g, e1 :> es) =>
  Bool ->
  Int ->
  Random g e1 ->
  Eff es ByteArray
uniformByteArrayM = RS.uniformByteArrayM
-}

uniformByteStringM ::
  (RandomGen g, e1 :> es) =>
  Int ->
  Random g e1 ->
  Eff es ByteString
uniformByteStringM = RS.uniformByteStringM

uniformShortByteStringM ::
  (RandomGen g, e1 :> es) =>
  Int ->
  Random g e1 ->
  Eff es ShortByteString
uniformShortByteStringM = RS.uniformShortByteStringM

uniformDouble01M ::
  (RandomGen g, e1 :> es) =>
  Random g e1 ->
  Eff es Double
uniformDouble01M = RS.uniformDouble01M

uniformDoublePositive01M ::
  (RandomGen g, e1 :> es) =>
  Random g e1 ->
  Eff es Double
uniformDoublePositive01M = RS.uniformDouble01M

uniformFloat01M ::
  (RandomGen g, e1 :> es) =>
  Random g e1 ->
  Eff es Float
uniformFloat01M = RS.uniformFloat01M

uniformFloatPositive01M ::
  (RandomGen g, e1 :> es) =>
  Random g e1 ->
  Eff es Float
uniformFloatPositive01M = RS.uniformFloat01M
