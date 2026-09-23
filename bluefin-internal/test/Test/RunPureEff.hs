{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Test.RunPureEff where

import Bluefin.Internal
import Control.Concurrent (threadDelay, throwTo)
import Control.Concurrent.Async (asyncThreadId, waitCatch, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, evaluate)
import Control.Exception qualified as Exception
import Control.Monad (forever, unless)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import System.Mem (performMajorGC)
import System.Timeout (timeout)
import Test.SpecH (SpecH, assertSatisfies)

-- Interrupt a thread while it forces a shared thunk which runs inside
-- a bracket, then force the thunk again and check from what point the
-- computation resumed.
test_runPureEffAsyncSafeSurvivesInterruptedBracket ::
  (forall r. (forall e. Eff e r) -> r) ->
  IO InterruptedBracketResult
test_runPureEffAsyncSafeSurvivesInterruptedBracket run = do
  started <- newEmptyMVar
  continue <- newEmptyMVar
  forceThunk <- do
    modified <- newIORef False
    ref <- newIORef $ run $ bracket (pure ()) (\() -> pure ()) $ \() -> do
      let prog = do
            previouslyModified <-
              atomicModifyIORef' modified $ \previous ->
                (True, previous)
            -- Unless the IORef was previously modified, this is our
            -- first run. Therefore, wait for the caller to unblock us,
            -- and throw us an asynchronous exception.
            unless previouslyModified $ do
              _ <- tryPutMVar started ()
              takeMVar continue
            pure previouslyModified
      unsafeProvideIO (\io -> effIO io prog)
    pure (evaluate =<< readIORef ref)
  interrupted <- withAsync forceThunk $ \worker -> do
    takeMVar started
    throwTo (asyncThreadId worker) ThreadKilled
    waitCatch worker
  case interrupted of
    Left eInterrupted
      | Just ThreadKilled <- Exception.fromException eInterrupted -> do
          resumed <- do
            -- If the first evaluation is resumed, it will need
            -- unblocking.  If evaluation started from scratch it
            -- won't block on continue anyway, because
            -- previouslyModified is true.
            putMVar continue ()
            Exception.try @SomeException $ do
              forceThunk
          case resumed of
            Left eResumed
              | Just ThreadKilled <- Exception.fromException eResumed ->
                  pure ThunkPoisoned
              | otherwise ->
                  pure (UnexpectedException eResumed)
            Right previouslyModified ->
              pure $ case previouslyModified of
                False -> ThunkClean
                True -> RanAtLeastTwice
      | otherwise -> pure (UnexpectedException eInterrupted)
    Right _ -> pure FinishedEarly

-- Drop the result thunk after cancelling its forcing thread, then collect it
-- and check that its weak finalizer kills the computation worker.
test_runPureEffAsyncSafeReapsWorker :: IO (Maybe AsyncException)
test_runPureEffAsyncSafeReapsWorker = do
  started <- newEmptyMVar
  caught <- newEmptyMVar
  (release, forceThunk) <- do
    shared <- newIORef @(Maybe ()) $ Just $ runPureEffAsyncSafe $ do
      unsafeProvideIO $ \io -> do
        effIO io $ do
          putMVar started ()
          Exception.handle @SomeException
            (putMVar caught)
            (forever (threadDelay 1_000_000))
    pure
      ( writeIORef shared Nothing,
        maybe (fail "result thunk released") evaluate =<< readIORef shared
      )
  withAsync forceThunk $ \forcingThunk -> do
    takeMVar started
    throwTo (asyncThreadId forcingThunk) ThreadKilled
    waitCatch forcingThunk >>= \case
      Right () ->
        fail "forcing thunk completed instead of being killed"
      Left e
        | -- We expect forcingThurk to be killed by ThreadKilled,
          -- because that's what we just threw to it.
          Just ThreadKilled <- Exception.fromException e ->
            pure ()
        | -- If we were killed by anything else, that should be
          -- reported
          otherwise ->
            Exception.throwIO e

  release
  performMajorGC
  caughtException <- timeout 1000000 (takeMVar caught)
  pure (Exception.fromException =<< caughtException)

data InterruptedBracketResult
  = FinishedEarly
  | UnexpectedException !SomeException
  | ThunkPoisoned
  | ThunkClean
  | RanAtLeastTwice
  deriving stock (Show)

isClean :: InterruptedBracketResult -> Bool
isClean = \case
  ThunkClean -> True
  _ -> False

isPoisoned :: InterruptedBracketResult -> Bool
isPoisoned = \case
  ThunkPoisoned -> True
  _ -> False

isRanAtLeastTwice :: InterruptedBracketResult -> Bool
isRanAtLeastTwice = \case
  RanAtLeastTwice -> True
  _ -> False

assertInterruptedBracketOutcome ::
  (e1 <: es, e2 <: es) =>
  IOE e1 ->
  SpecH e2 ->
  String ->
  (InterruptedBracketResult -> Bool) ->
  (forall r. (forall e. Eff e r) -> r) ->
  Eff es ()
assertInterruptedBracketOutcome io y name predicate run = do
  actual <- effIO io $ test_runPureEffAsyncSafeSurvivesInterruptedBracket run
  assertSatisfies y name predicate actual
