module Bluefin.Examples.Cont where

import Bluefin.Cont
import Bluefin.IO (runEff)
import Bluefin.State
import Bluefin.System.IO (hPutStr, withFile)
import System.IO (IOMode (ReadMode))

f :: IO Integer
f = runEff $ \io -> do
  runCont $ \eff -> do
    h <- new eff $ withFile io "file.txt" ReadMode
    s1 <- new eff $ evalState 5
    s2 <- new eff $ evalState 10
    liftEff eff $ do
      s <- get s1
      modify s2 (+ s)
      hPutStr h "hello world"
      get s2
