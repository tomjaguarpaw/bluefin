{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Bluefin.Internal.Transformers where

import Bluefin.Internal hiding (b, w)
import Bluefin.Internal.EffReaderList
  ( EffReaderList,
    Finite,
    abstract,
    apply',
    effReaderList,
    runEffReaderList,
    withRunInEff, apply, mapEffReaderListEffect, runInEff', withRunInEff',
  )
import Control.Category ((>>>))
import Control.Monad (when)
import Control.Monad.Morph (MFunctor, hoist)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer

toState ::
  (Finite hs) =>
  EffReaderList (State s : hs) es a ->
  -- | ͘
  State.StateT s (EffReaderList hs es) a
toState b = State.StateT $ \s -> do
  withRunInEff' $ \rie -> do
    runState s $ \st -> do
      runInEff' rie $ do
        apply' b st

example :: EffReaderList [Exception String, State Int, Reader Bool] es ()
example =
  abstract $ \ex -> abstract $ \st -> abstract $ \re -> effReaderList $ do
    r <- ask re
    n1 <- get st
    when (n1 > 42) $ throw ex ("First check; " <> show r)
    put st (n1 + 1)
    n2 <- get st
    when (n2 > 42) $ throw ex ("Second check; " <> show r)
    put st (n2 + 1)

example1 ::
  EffReaderList [Exception String, State Int, Reader Bool] es () ->
  Except.ExceptT String (State.StateT Int (Reader.ReaderT Bool (Eff es))) ()
example1 =
  toExcept `bar` (toState `bar` (toReader `bar` runEffReaderList))

bar ::
  (MFunctor t, Monad m) =>
  (a -> t m b) ->
  (forall z. m z -> n z) ->
  (a -> t n b)
x `bar` y = x >>> hoist y

runExample :: (Either String (), Int)
runExample =
  runPureEff $
    flip Reader.runReaderT True $
      flip State.runStateT 42 $
        Except.runExceptT $
          example1 example

toExcept ::
  (Finite hs) =>
  EffReaderList (Exception s : hs) es a ->
  -- | ͘
  Except.ExceptT s (EffReaderList hs es) a
toExcept b = Except.ExceptT $ do
  withRunInEff $ \runInEff -> do
    try $ \ex -> do
      foo $ do
        runInEff $ do
          apply' b ex

toReader ::
  (Finite hs) =>
  EffReaderList (Reader r : hs) es a ->
  -- | ͘
  Reader.ReaderT r (EffReaderList hs es) a
toReader b = Reader.ReaderT $ \r -> do
  withRunInEff $ \runInEff -> do
    runReader r $ \re -> do
      foo $ do
        runInEff $ do
          apply' b re

toWriter ::
  (Finite hs, Monoid w) =>
  EffReaderList (Writer w : hs) es a ->
  -- | ͘
  Writer.WriterT w (EffReaderList hs es) a
toWriter b = Writer.WriterT $ do
  withRunInEff $ \runInEff -> do
    runWriter $ \re -> do
      foo $ do
        runInEff $ do
          apply' b re

foo :: Eff (e :& (es :& e1)) a -> Eff (e1 :& (e :& es)) a
foo = weakenEff (withBase $ \base -> bimap has (swap base) `cmp` assoc2 base `cmp` bimap (swap base) has `cmp` assoc1 base)
