module Bluefin.Examples.Reader where

import Bluefin.Consume (await, streamConsume)
import Bluefin.IO (effIO, runEff_)
import Bluefin.Reader (ask, local, runReader)
import Bluefin.Stream (yield)

-- ghci> main
-- Should be 0: 0
-- Should be 0: -100
-- Should be 1: -99
-- Should be 1: 0
-- Should be 0: -100
main :: IO ()
main = runEff_ $ \io -> do
  runReader @Int 0 $ \r -> do
    streamConsume
      ( \y -> do
          let s = yield y =<< ask r
          s
          s
          local r (+ 1) $ do
            s
            s
          s
      )
      ( \a -> do
          let p m = do
                v <- await a
                effIO io (putStrLn ("Should be " <> m <> ": " <> show v))
          p "0"
          local r (subtract 100) $ do
            p "0"
            p "1"
          p "1"
          p "0"
      )
