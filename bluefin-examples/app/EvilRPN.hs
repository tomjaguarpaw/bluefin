module Main where

import Bluefin.Contrib.Application
import Bluefin.Examples.EvilRPN.Application
import Bluefin.Examples.EvilRPN.Language
import Bluefin.Examples.EvilRPN.Store
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Default
import Data.Function
import Data.IORef

main :: IO ()
main = do
  writeFile "evilrpn" $ show $ EvilStack mempty mempty
  continue <- newIORef True
  res <- withApplicationIO
    (ApplicationState 0 0)
    (def {testFakeStart = Nothing})
    (runStoreIO "evilrpn")
    mainLoop $ \terminate output input ->
    bracket
    (do
        readOutputThread <- forkIO
          (forever $ readChan output >>=
           either (\e -> do
                      print e
                      writeIORef continue False
                      terminate
                  ) print)
        pure [readOutputThread]
    )
    (mapM_ killThread
    ) $ const $ fix $ \loop -> do
    getLine >>= \case
      "q" -> pure ()
      l -> do
        maybe (putStrLn "no parse!") (writeChan input) $ parse l
        c <- readIORef continue
        when c loop
  print res
