{-# LANGUAGE NoMonoLocalBinds #-}

module Bluefin.Internal.Concurrent where

import Bluefin.Internal
import Control.Concurrent (MVar, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever, replicateM_, when)
import Data.Functor (void)
import qualified Data.List as List
import Data.Maybe (isJust)
import Data.Proxy (Proxy (Proxy))
import qualified Ki
import System.IO.Unsafe (unsafePerformIO)

data ExclusiveAccess (es' :: Effects) (es :: Effects)
  = UnsafeMkExclusiveAccess (MVar ()) (STME es)

instance Handle (ExclusiveAccess es') where
  mapHandle (UnsafeMkExclusiveAccess v stm) = UnsafeMkExclusiveAccess v (mapHandle stm)

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
  unsafeUnEff (k (UnsafeMkScope scope (UnsafeMkExclusiveAccess lock UnsafeMkSTME)))

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
exclusively (UnsafeMkExclusiveAccess lock _) body = do
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
fork (UnsafeMkScope scope (UnsafeMkExclusiveAccess lock stm)) body = do
  thread <- UnsafeMkEff $ Ki.fork scope $ do
    unsafeUnEff (body (UnsafeMkExclusiveAccess lock stm))
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

instance Handle STME where
  mapHandle UnsafeMkSTME = UnsafeMkSTME

effSTM ::
  (e :> es) =>
  STME e ->
  STM.STM a ->
  -- | ͘
  EffSTM es a
effSTM UnsafeMkSTME = UnsafeMkEffSTM

newtype TChan a (e :: Effects) = UnsafeMkTChan (STM.TChan a)

atomicallySTM ::
  (e1 :> es) =>
  STME e1 ->
  STM.STM a ->
  -- | ͘
  Eff es a
atomicallySTM UnsafeMkSTME stm = UnsafeMkEff (STM.atomically stm)

atomically ::
  (e1 :> es) =>
  IOE e1 ->
  -- | ͘
  (forall e. STME e -> EffSTM e r) ->
  Eff es r
atomically MkIOE body =
  UnsafeMkEff (STM.atomically (unsafeUnEffSTM (body UnsafeMkSTME)))

atomicallyPure ::
  EffSTM e r ->
  -- | ͘
  r
atomicallyPure body =
  unsafePerformIO (STM.atomically (unsafeUnEffSTM body))

withTChan ::
  (forall e. TChan a e -> EffSTM (e :& es) r) ->
  EffSTM es r
withTChan body = UnsafeMkEffSTM $ do
  chan <- STM.newTChan
  unsafeUnEffSTM $ body $ UnsafeMkTChan chan

readTChan ::
  (e :> es) =>
  TChan a e ->
  EffSTM es a
readTChan (UnsafeMkTChan chan) = UnsafeMkEffSTM $ do
  STM.readTChan chan

writeTChan ::
  (e :> es) =>
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

voidThread :: (Functor m) => m (t () e) -> m ()
voidThread = fmap (\_ -> ())

runSTM ::
  (e1 :> es) =>
  IOE e1 ->
  (forall e. STME e -> Eff (e :& es) r) ->
  Eff es r
runSTM MkIOE body = makeOp (body UnsafeMkSTME)

accessSTME ::
  (e1' :> es', e1 :> es) =>
  ExclusiveAccess es' e1 ->
  STME e1' ->
  Eff es (STME e1)
accessSTME (UnsafeMkExclusiveAccess _ _) UnsafeMkSTME =
  pure UnsafeMkSTME

accessSTME2 ::
  (e1'' :> es'', es'1 :> es', e1 :> es) =>
  ExclusiveAccess es'' es'1 ->
  ExclusiveAccess es' e1 ->
  STME e1'' ->
  Eff es (STME (e1 :: Effects))
accessSTME2 excl1 excl2 stm = do
  r <- exclusively excl2 $ do
    r <- accessSTME excl1 stm

    pure r

  accessSTME excl2 r

example :: IO ()
example = runEff $ \io -> runSTM io $ \stm -> do
  Proxy :: Proxy e <- getEffStack

  let notThreadSafe :: Int -> Eff e ()
      notThreadSafe i = effIO io $ do
        putStrLn ("Entering " ++ show i)
        threadDelay 1_000
        putStrLn ("Exiting " ++ show i)

  chan <- effIO io STM.newTChanIO

  scoped $ \scope -> do
    t <- fork scope $ \excl -> withJump $ \j -> do
      stm' <- accessSTME excl stm

      forever $ do
        atomicallySTM stm' (STM.readTChan chan) >>= \case
          Nothing -> jumpTo j
          Just a -> do
            exclusively excl $ do
              effIO io (putStrLn a)

    evalState () $ \stTop -> do
      scoped $ \scope1 -> do
        voidThread $ fork scope1 $ \excl1 -> do
          -- This seems too complicated
          stm' <- do
            stm' <- exclusively excl1 $ do
              excl <- exclusiveAccessOfScopeEff scope
              accessSTME excl stm

            accessSTME excl1 stm'

          replicateM_ 10 $ do
            atomicallySTM stm' $ STM.writeTChan chan (Just "Hello")

          atomicallySTM stm' (STM.writeTChan chan Nothing)

        t1 <- fork scope1 $ \excl1 -> do
          exclusively excl1 $ get stTop

          evalState @Int 0 $ \st -> do
            scoped $ \scope2 -> do
              t2 <- fork scope2 $ \excl2 -> do
                replicateM_ 3 $ do
                  exclusively excl2 $ do
                    modify st (+ 1)
                    exclusively excl1 $ do
                      excl <- exclusiveAccessOfScopeEff scope
                      exclusively excl $ do
                        notThreadSafe 1

              scope2' <- exclusiveAccessOfScopeEff scope2
              exclusively scope2' $ put st 0

              t3 <- fork scope2 $ \excl2 -> do
                replicateM_ 3 $ do
                  exclusively excl2 $ do
                    modify st (+ 1)
                    exclusively excl1 $ do
                      excl <- exclusiveAccessOfScopeEff scope
                      exclusively excl $ do
                        notThreadSafe 2

              awaitEff t2
              awaitEff t3

        awaitEff t1

    awaitEff t

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
