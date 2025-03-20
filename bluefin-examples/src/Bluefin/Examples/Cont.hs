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
      put s1 10
      put s2 15
      hPutStr h "hello world"
      get s2
