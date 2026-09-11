{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Test.RunPureEff where

import Bluefin.Internal
import Control.Concurrent (threadDelay, throwTo)
import Control.Concurrent.Async (asyncThreadId, waitCatch, withAsync)
import Control.Exception (AsyncException (ThreadKilled), SomeException, evaluate)
import Control.Exception qualified as Exception
import Data.Foldable (for_)
import Test.SpecH (SpecH, assertEqual)

test_runPureEffAsyncSafeSurvivesInterruptedBracket ::
  (forall r. (forall e. Eff e r) -> r) ->
  IO InterruptedBracketResult
test_runPureEffAsyncSafeSurvivesInterruptedBracket run = do
  let iterations :: Int
      iterations = 10_000_000
      result = run $
        bracket
          (pure ())
          (\() -> pure ())
          ( \() ->
              evalState (0 :: Int) $ \state -> do
                for_ [1 .. iterations] $ \_ -> modify state (+ 1)
                get state
          )
  interrupted <- withAsync (result `seq` pure ()) $ \worker -> do
    threadDelay 20_000
    throwTo (asyncThreadId worker) ThreadKilled
    waitCatch worker
  case interrupted of
    Left e
      | Just ThreadKilled <- Exception.fromException e -> do
          resumed <- Exception.try @SomeException (evaluate result)
          case resumed of
            Left resumedException
              | Just ThreadKilled <- Exception.fromException resumedException ->
                  pure ThunkPoisoned
              | otherwise ->
                  pure UnexpectedException
            Right actual
              | actual == iterations -> pure ThunkClean
              | otherwise -> pure (UnexpectedResult actual)
      | otherwise -> pure UnexpectedException
    Right _ -> pure FinishedEarly

data InterruptedBracketResult
  = FinishedEarly
  | UnexpectedException
  | ThunkPoisoned
  | ThunkClean
  | UnexpectedResult !Int
  deriving stock (Eq, Show)

assertInterruptedBracketOutcome ::
  (e1 <: es, e2 <: es) =>
  IOE e1 ->
  SpecH e2 ->
  String ->
  InterruptedBracketResult ->
  (forall r. (forall e. Eff e r) -> r) ->
  Eff es ()
assertInterruptedBracketOutcome io y name expected run = do
  actual <- effIO io $ test_runPureEffAsyncSafeSurvivesInterruptedBracket run
  assertEqual y name expected actual
