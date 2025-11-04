import Bluefin.Internal
import Control.Monad (replicateM_)
import Control.Monad.Trans.State qualified as Trans.State
import Criterion.Main (bench, bgroup, defaultMain, whnf, whnfIO)
import Data.Foldable (for_)
import Data.IORef
import Effectful qualified
import Effectful.State.Static.Local qualified as Effectful.State

benchBluefin :: Int -> Int
benchBluefin n = fst $ snd $ runPureEff $ runState (0 :: Int, 1 :: Int) $ \st -> do
  replicateM_ n (modify' st $ \(!cur, !next) -> (next, cur + next))

benchBluefinOld :: Int -> Int
benchBluefinOld n = fst $ snd $ runPureEff $ runState (0 :: Int, 1 :: Int) $ \st -> do
  replicateM_ n (modify st $ \(!cur, !next) -> (next, cur + next))

benchBluefinGetPut :: Int -> Int
benchBluefinGetPut n = fst $ snd $ runPureEff $ runState (0 :: Int, 1 :: Int) $ \st -> do
  replicateM_ n $ do
    (!cur, !next) <- get st
    put st (next, cur + next)

benchIORef :: Int -> IO Int
benchIORef n =
  fst <$> do
    ref <- newIORef @(Int, Int) (0, 1)
    replicateM_ n $ go ref
    readIORef ref
  where
    go !ref = do
      modifyIORef' ref $ \(!cur, !next) -> (next, cur + next)

benchIORefGetPut :: Int -> IO Int
benchIORefGetPut n =
  fst <$> do
    ref <- newIORef @(Int, Int) (0, 1)
    replicateM_ n $ go ref
    readIORef ref
  where
    go !ref = do
      (!cur, !next) <- readIORef ref
      writeIORef ref (next, cur + next)

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

benchMain :: IO ()
benchMain = do
  let n = 5000000
  defaultMain
    [ bgroup
        "my benchmarks"
        [ bench "bluefin state old" $ whnf benchBluefinOld n,
          bench "bluefin state getput" $ whnf benchBluefinGetPut n,
          bench "bluefin state" $ whnf benchBluefin n,
          bench "IORef" $ whnfIO (benchIORef n),
          bench "IORefGetPut" $ whnfIO (benchIORefGetPut n)
--          bench "effectful state" $ whnf benchEffectful n,
--          bench "trans" $ whnf benchTrans n
        ]
    ]

justTest :: IO ()
justTest = do
  let l = [0 .. 20] :: [Int]
  let fs =
        map
          (`map` l)
          [ return <$> benchBluefin --,
--            return <$> benchBluefinOld,
--            return <$> benchTrans,
--            benchIORef,
--            return <$> benchEffectful
          ]
  results <- mapM sequence fs
  for_ results $ \result -> print result

main :: IO ()
main = do
  justTest
  benchMain
