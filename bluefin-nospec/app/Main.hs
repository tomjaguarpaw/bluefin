{-# LANGUAGE BangPatterns #-}

import Bluefin.Internal
import Data.IORef
import Control.Monad

benchBluefin :: Int -> Int
benchBluefin n = fst $ snd $ runPureEff $ runState (0 :: Int, 1 :: Int) $ \st -> do
  replicateM_ n (modify' st $ \(!cur, !next) -> (next, cur + next))

benchIORef :: Int -> IO Int
benchIORef n =
  fst <$> do
    ref <- newIORef (0, 1)
    replicateM_ n $ go ref
    readIORef ref
  where
    go !ref = do
      modifyIORef' ref $ \(!cur, !next) -> (next, cur + next)

main :: IO ()
main = do
  print (benchBluefin 10)
  print =<< benchIORef 10
