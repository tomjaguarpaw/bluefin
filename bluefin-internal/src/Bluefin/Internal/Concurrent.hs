{-# LANGUAGE NoMonoLocalBinds #-}

module Bluefin.Internal.Concurrent where

import Bluefin.Internal
import Control.Concurrent (MVar, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Monad (replicateM_, when)
import Control.Monad.STM (atomically)
import Data.Functor (void)
import qualified Data.List as List
import Data.Maybe (isJust)
import Data.Proxy (Proxy (Proxy))
import qualified Ki

data ExclusiveAccess (e1 :: Effects) (e2 :: Effects)
  = UnsafeMkExclusiveAccess (MVar ())

newtype Scope (e :: Effects) = UnsafeMkScope Ki.Scope

newtype Thread (e :: Effects) r
  = UnsafeMkThread (Ki.Thread r)

scoped ::
  (forall e. Scope e -> ExclusiveAccess es e -> Eff e r) ->
  -- | ͘
  Eff es r
scoped k = UnsafeMkEff $ Ki.scoped $ \scope -> do
  -- Unlocked when it's empty
  lock <- newEmptyMVar
  unsafeUnEff (k (UnsafeMkScope scope) (UnsafeMkExclusiveAccess lock))

exclusively ::
  e1 :> es =>
  ExclusiveAccess e e1 ->
  Eff e r ->
  -- | ͘
  Eff es r
exclusively (UnsafeMkExclusiveAccess lock) body = do
  () <- UnsafeMkEff (putMVar lock ())
  r <- UnsafeMkEff (unsafeUnEff body)
  UnsafeMkEff (takeMVar lock)
  pure r

fork ::
  (e1 :> es, e2 :> es) =>
  Scope e1 ->
  ExclusiveAccess es' e2 ->
  (forall e. ExclusiveAccess es' e -> Eff e r) ->
  -- | ͘
  Eff es (Thread es r)
fork (UnsafeMkScope scope) (UnsafeMkExclusiveAccess lock) body = do
  thread <- UnsafeMkEff $ Ki.fork scope $ do
    unsafeUnEff (body (UnsafeMkExclusiveAccess lock))
  pure (UnsafeMkThread thread)

awaitEff :: Thread e a -> Eff es a
awaitEff (UnsafeMkThread t) =
  UnsafeMkEff (atomically (Ki.await t))

getEffStack :: Eff es (Proxy es)
getEffStack = pure Proxy

example :: IO ()
example = runEff $ \io -> do
  Proxy :: Proxy e <- getEffStack

  let notThreadSafe :: Int -> Eff e ()
      notThreadSafe i = effIO io $ do
        putStrLn ("Entering " ++ show i)
        threadDelay 1_000
        putStrLn ("Exiting " ++ show i)

  scoped $ \scope1 excl1 -> do
    t1 <- fork scope1 excl1 $ \excl2 -> do
      evalState @Int 0 $ \st -> do
        scoped $ \scope2 excl3 -> do
          t2 <- fork scope2 excl3 $ \excl4 -> do
            replicateM_ 3 $ do
              exclusively excl4 $ do
                modify st (+ 1)
                exclusively excl2 $
                  notThreadSafe 1

          t3 <- fork scope2 excl3 $ \excl4 -> do
            replicateM_ 3 $ do
              exclusively excl4 $ do
                modify st (+ 1)
                exclusively excl2 $
                  notThreadSafe 2

          awaitEff t2
          awaitEff t3

    awaitEff t1

staggeredSpawner :: [IO ()] -> IO ()
staggeredSpawner actions = do
  Ki.scoped $ \scope -> do
    actions
      & map (\action -> void (Ki.fork scope action))
      & List.intersperse (threadDelay 250_000)
      & sequence_
    atomically (Ki.awaitAll scope)
  where
    (&) = flip ($)

happyEyeballs :: [IO (Maybe a)] -> IO (Maybe a)
happyEyeballs actions = do
  resultVar <- newEmptyMVar

  let worker action = do
        result <- action
        when (isJust result) $ do
          _ <- tryPutMVar resultVar result
          pure ()

  Ki.scoped $ \scope -> do
    _ <-
      Ki.fork scope $ do
        staggeredSpawner (map worker actions)
        tryPutMVar resultVar Nothing
    takeMVar resultVar

staggeredSpawnerBf :: [IO ()] -> IO ()
staggeredSpawnerBf actions = do
  Ki.scoped $ \scope -> do
    actions
      & map (\action -> void (Ki.fork scope action))
      & List.intersperse (threadDelay 250_000)
      & sequence_
    atomically (Ki.awaitAll scope)
  where
    (&) = flip ($)

happyEyeballsBf :: [IO (Maybe a)] -> IO (Maybe a)
happyEyeballsBf actions = do
  resultVar <- newEmptyMVar

  let worker action = do
        result <- action
        when (isJust result) $ do
          _ <- tryPutMVar resultVar result
          pure ()

  Ki.scoped $ \scope -> do
    _ <-
      Ki.fork scope $ do
        staggeredSpawner (map worker actions)
        tryPutMVar resultVar Nothing
    takeMVar resultVar
