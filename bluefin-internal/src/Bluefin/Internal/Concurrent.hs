{-# LANGUAGE NoMonoLocalBinds #-}

module Bluefin.Internal.Concurrent where

import Bluefin.Internal
import Control.Concurrent (MVar, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad (replicateM_, when)
import Data.Functor (void)
import qualified Data.List as List
import Data.Maybe (isJust)
import Data.Proxy (Proxy (Proxy))
import qualified Ki

newtype ExclusiveAccess (es' :: Effects) (es :: Effects)
  = UnsafeMkExclusiveAccess (MVar ())

instance Handle (ExclusiveAccess es') where
  mapHandle (UnsafeMkExclusiveAccess v) = UnsafeMkExclusiveAccess v

data Scope (es' :: Effects) (es :: Effects)
  = UnsafeMkScope Ki.Scope (ExclusiveAccess es' es)

instance Handle (Scope es') where
  mapHandle (UnsafeMkScope scope excl) = UnsafeMkScope scope (mapHandle excl)

newtype Thread a (e :: Effects)
  = UnsafeMkThread (Ki.Thread a)

instance Handle (Thread r) where
  mapHandle (UnsafeMkThread t) = UnsafeMkThread t

scoped ::
  (forall e. Scope es e -> Eff e r) ->
  -- | ͘
  Eff es r
scoped k = UnsafeMkEff $ Ki.scoped $ \scope -> do
  -- Unlocked when it's empty
  lock <- newEmptyMVar
  unsafeUnEff (k (UnsafeMkScope scope (UnsafeMkExclusiveAccess lock)))

exclusiveAccessOfScopeEff ::
  (e1 :> es) =>
  Scope es' e1 ->
  -- | ͘
  Eff es (ExclusiveAccess es' e1)
exclusiveAccessOfScopeEff (UnsafeMkScope _ excl) = pure excl

exclusively ::
  (e1 :> es) =>
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
  (e1 :> es) =>
  Scope es' e1 ->
  (forall e. ExclusiveAccess es' e -> Eff e r) ->
  -- | ͘
  Eff es (Thread r es)
fork (UnsafeMkScope scope (UnsafeMkExclusiveAccess lock)) body = do
  thread <- UnsafeMkEff $ Ki.fork scope $ do
    unsafeUnEff (body (UnsafeMkExclusiveAccess lock))
  pure (UnsafeMkThread thread)

awaitEff ::
  Thread a e ->
  -- | ͘
  Eff es a
awaitEff (UnsafeMkThread t) =
  -- This is safe, presumably, because it doesn't change any STM
  -- variables observably, presumably.
  UnsafeMkEff (STM.atomically (Ki.await t))

getEffStack :: Eff es (Proxy es)
getEffStack = pure Proxy

newtype EffSTM (e :: Effects) a = UnsafeMkEffSTM {unsafeUnEffSTM :: STM.STM a}

data STME (e :: Effects) = UnsafeMkSTME

effSTM ::
  (e :> es) =>
  STME e ->
  STM.STM a ->
  -- | ͘
  EffSTM es a
effSTM UnsafeMkSTME = UnsafeMkEffSTM

newtype TChan a (e :: Effects) = UnsafeMkTChan (STM.TChan a)

withTChan ::
  (forall e. TChan a e -> EffSTM (e :& es) r) ->
  EffSTM es r
withTChan body = UnsafeMkEffSTM $ do
  chan <- STM.newTChan
  unsafeUnEffSTM $ body $ UnsafeMkTChan chan

readTChan ::
  e :> es =>
  TChan a e ->
  EffSTM es a
readTChan (UnsafeMkTChan chan) = UnsafeMkEffSTM $ do
  STM.readTChan chan

writeTChan ::
  e :> es =>
  TChan a e ->
  a ->
  EffSTM es ()
writeTChan (UnsafeMkTChan chan) a = UnsafeMkEffSTM $ do
  STM.writeTChan chan a

withAsync ::
  (forall e. ExclusiveAccess es e -> Eff e r1) ->
  (forall e. Scope es e -> Thread r1 e -> Eff e r2) ->
  -- | ͘
  Eff es r2
withAsync
  forkIt
  body = scoped $ \scope ->
    body scope =<< fork scope forkIt

example :: IO ()
example = runEff $ \io -> do
  Proxy :: Proxy e <- getEffStack

  let notThreadSafe :: Int -> Eff e ()
      notThreadSafe i = effIO io $ do
        putStrLn ("Entering " ++ show i)
        threadDelay 1_000
        putStrLn ("Exiting " ++ show i)

  evalState () $ \stTop -> do
    scoped $ \scope1 -> do
      t1 <- fork scope1 $ \excl1 -> do
        exclusively excl1 $ get stTop

        evalState @Int 0 $ \st -> do
          scoped $ \scope2 -> do
            t2 <- fork scope2 $ \excl2 -> do
              replicateM_ 3 $ do
                exclusively excl2 $ do
                  modify st (+ 1)
                  exclusively excl1 $
                    insertFirst $
                      notThreadSafe 1

            scope2' <- exclusiveAccessOfScopeEff scope2
            exclusively scope2' $ put st 0

            t3 <- fork scope2 $ \excl2 -> do
              replicateM_ 3 $ do
                exclusively excl2 $ do
                  modify st (+ 1)
                  exclusively excl1 $
                    insertFirst $
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
    STM.atomically (Ki.awaitAll scope)
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
    STM.atomically (Ki.awaitAll scope)
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
