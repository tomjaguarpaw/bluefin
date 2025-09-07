{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module Bluefin.Internal.Transformers where

import Bluefin.Internal hiding (b, w)
import Bluefin.Internal.EffReaderList
  ( EffReaderList,
    Finite,
    abstract,
    apply,
    effReaderList,
    effReaderListHandler,
    useImplG,
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
            useImplG b `apply` st `apply` stu `apply` ex

toState ::
  (Finite hs) =>
  EffReaderList (State s : hs) es a ->
  -- | ͘
  State.StateT s (EffReaderList hs es) a
toState b = State.StateT $ \s ->
  effReaderListHandler (runState s) b

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
  effReaderListHandler try b

toReader ::
  (Finite hs) =>
  EffReaderList (Reader r : hs) es a ->
  -- | ͘
  Reader.ReaderT r (EffReaderList hs es) a
toReader b = Reader.ReaderT $ \r -> do
  effReaderListHandler (runReader r) b

toWriter ::
  (Finite hs, Monoid w) =>
  EffReaderList (Writer w : hs) es a ->
  -- | ͘
  Writer.WriterT w (EffReaderList hs es) a
toWriter b =
  Writer.WriterT $
    effReaderListHandler runWriter b
