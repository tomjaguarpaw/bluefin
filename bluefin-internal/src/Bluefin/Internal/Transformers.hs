{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module Bluefin.Internal.Transformers where

import Bluefin.Internal hiding (b, w)
import Bluefin.Internal.EffReaderList
  ( EffReaderList,
    Finite,
    InEffRunner,
    abstract,
    apply,
    effReaderList,
    mapEffReaderListEffect,
    mapEffReaderListEffectIn,
    runEffReaderList,
    runInEff,
    withRunInEff,
  )
import Control.Category ((>>>))
import Control.Monad (when)
import Control.Monad.Morph (MFunctor, hoist)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer

toStateAndUnit ::
  (Finite hs, e :> es) =>
  EffReaderList (State s : State () : Exception exn : hs) e a ->
  State.StateT s (EffReaderList hs es) (Either exn a)
toStateAndUnit b = State.StateT $ \s -> do
  withRunInEff $ \rie -> do
    runState s $ \st -> do
      evalState () $ \stu -> do
        try $ \ex ->
          runInEff rie $ do
            ((mapEffReaderListEffect b `apply` st) `apply` stu) `apply` ex

runIn ::
  (Finite hs, e1 :> es, e2 :> es, e3 :> es) =>
  InEffRunner hs e1 ->
  EffReaderList (h : hs) e3 r ->
  h e2 ->
  Eff es r
runIn rie b h =
  runInEff rie $ do
    mapEffReaderListEffect b `apply` h

foo ::
  (Finite hs, e1 :> es, e3 :> es) =>
  ((forall e. h e -> Eff (e :& es) r) -> Eff es b) ->
  InEffRunner hs e1 ->
  EffReaderList (h : hs) e3 r ->
  Eff es b
foo k rie b =
  k $
    runIn rie b

foo1 ::
  (Finite hs, e1 :> es, e3 :> es) =>
  (EffReaderList '[h] es r -> Eff es b) ->
  InEffRunner hs e1 ->
  EffReaderList (h : hs) e3 r ->
  Eff es b
foo1 karg = foo (\kerl -> karg (abstract $ \h -> effReaderList (kerl h)))

blah :: ((forall e. h e -> Eff (e :& es) r) -> b2) -> (EffReaderList '[h] es r -> b2)
blah h erl = h $
  \st -> runEffReaderList (apply (mapEffReaderListEffectIn (withBase sndI) erl) st)

blaz ::
  (Finite hs, e3 :> es) =>
  (forall e. EffReaderList '[h] (e :& es) r1 -> Eff (e :& es) r2) ->
  EffReaderList (h : hs) e3 r1 ->
  EffReaderList hs es r2
blaz bl b =
  withRunInEff $ \rie ->
    foo1 bl rie b

transformersHandler ::
  (Finite hs, e3 :> es) =>
  (forall e. (forall e1. h e1 -> Eff (e1 :& (e :& es)) r1) -> Eff (e :& es) r2) ->
  EffReaderList (h : hs) e3 r1 ->
  EffReaderList hs es r2
transformersHandler h = blaz (blah h)

toState ::
  (Finite hs) =>
  EffReaderList (State s : hs) es a ->
  -- | ͘
  State.StateT s (EffReaderList hs es) a
toState b = State.StateT $ \s ->
  transformersHandler (runState s) b

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
  transformersHandler try b

toReader ::
  (Finite hs) =>
  EffReaderList (Reader r : hs) es a ->
  -- | ͘
  Reader.ReaderT r (EffReaderList hs es) a
toReader b = Reader.ReaderT $ \r -> do
  transformersHandler (runReader r) b

toWriter ::
  (Finite hs, Monoid w) =>
  EffReaderList (Writer w : hs) es a ->
  -- | ͘
  Writer.WriterT w (EffReaderList hs es) a
toWriter b =
  Writer.WriterT $
    transformersHandler runWriter b
