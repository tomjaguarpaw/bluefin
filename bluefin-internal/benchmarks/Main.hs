import Bluefin.Internal
import Control.Monad (replicateM_)
import Criterion.Main ( defaultMain, bench, bgroup, whnf, whnfIO )
import Data.IORef
import qualified Effectful
import Effectful.State.Static.Local qualified as Effectful.State
import Control.Monad.Trans.State qualified as Trans.State

benchBluefin :: Int -> Int
benchBluefin n = fst $ snd $ runPureEff $ runState (0 :: Int, 1 :: Int) $ \st -> do
  replicateM_ n (go st)
  where
    go !ref = do
      modify' ref $ \(!cur, !next) -> (next, cur + next)

benchIORef :: Int -> IO Int
benchIORef n =
  fst <$> do
    ref <- newIORef @(Int, Int) (0, 1)
    replicateM_ n $ go ref
    readIORef ref
  where
    go !ref = do
      modifyIORef' ref $ \(!cur, !next) -> (next, cur + next)

benchEffectful :: Int -> Int
benchEffectful n = fst $
  Effectful.runPureEff $
    Effectful.State.execState @(Int, Int) (0, 1) $
      replicateM_ n $ do
        (!cur, !next) <- Effectful.State.get
        Effectful.State.put @(Int, Int) (next, cur + next)

benchTrans :: Int -> Int
benchTrans n = fst $ Trans.State.execState go (0, 1)
  where
    go =
      replicateM_ n $ do
        (!cur, !next) <- Trans.State.get
        Trans.State.put (next, cur + next)

main :: IO ()
main = do
  let n = 5000000
  defaultMain
    [ bgroup
        "my benchmarks"
        [ bench "bluefin state" $ whnf benchBluefin n,
          bench "IORef" $ whnfIO (benchIORef n),
          bench "effectful state" $ whnf benchEffectful n,
          bench "trans" $ whnf benchTrans n
        ]
    ]
