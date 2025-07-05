{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Bluefin.Internal.Transformers where

import Bluefin.Internal hiding (b, w)
import Bluefin.Internal.EffReaderList
import Control.Arrow ((<<<))
import Control.Category ((>>>))
import Control.Monad (when)
import Control.Monad.Morph (MFunctor, hoist)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import Debug.Trace (trace, traceM)

{-
bar ::
  (Handle h1, Handle h2, e1 :> es1, e2 :> es1, es2 :> es1) =>
  BetterEffReader (Product h1 h2) es2 r ->
  h1 e1 ->
  h2 e2 ->
  Eff es1 r
bar b h hs = do
  case b of
    MkBetterEffReader k -> do
      curryP (useImplUnder . k) h hs
-}

toState ::
  (Finite hs) =>
  EffReaderList (State s : hs) es a ->
  State.StateT s (EffReaderList hs es) a
toState b = State.StateT $ \s -> do
  traceIt "A" $ do
    withRunInEff $ \runInEff -> do
      runState s $ \st -> do
        traceIt "B" $ do
          foo $ do
            traceIt "C" $ do
              runInEff $ do
                traceIt "D" $ do
                  apply'' b st

example :: EffReaderList [Exception String, State Int] es ()
example = abstract $ \ex -> abstract $ \st -> effReaderList $ do
  n1 <- get st
  when (n1 > 42) $ throw ex "First check"
  put st (n1 + 1)
  n2 <- get st
  when (n2 > 42) $ throw ex "Second check"
  put st (n2 + 1)

example1 ::
  EffReaderList [Exception String, State Int] es () ->
  Except.ExceptT String (State.StateT Int (Eff es)) ()
example1 =
  toExcept `bar` (toState `bar` runEffReaderList)

bar ::
  (MFunctor t, Monad m) =>
  (a -> t m b) ->
  (forall z. m z -> n z) ->
  (a -> t n b)
x `bar` y = x >>> hoist y

runExample :: (Either String (), Int)
runExample =
  runPureEff $
    flip State.runStateT 42 $
      Except.runExceptT $
        example1 example

toExcept ::
  (Finite hs) =>
  EffReaderList (Exception s : hs) es a ->
  Except.ExceptT s (EffReaderList hs es) a
toExcept b = Except.ExceptT $ do
  withRunInEff $ \runInEff -> do
    try $ \ex -> do
      foo $ do
        runInEff $ do
          apply'' b ex

foo :: Eff (e :& (es :& e1)) a -> Eff (e1 :& (e :& es)) a
foo = weakenEff (withBase $ \base -> bimap has (swap base) `cmp` assoc2 base `cmp` bimap (swap base) has `cmp` assoc1 base)

{-
toExcept ::
  (Handle h) =>
  BetterEffReader (Product (Exception ex) h) es a ->
  Except.ExceptT ex (BetterEffReader h es) a
toExcept b = Except.ExceptT $ do
  MkBetterEffReader $ \hs -> do
    try $ \ex -> do
      bar b ex hs

toWriter ::
  (Handle h, Monoid w) =>
  BetterEffReader (Product (Writer w) h) es a ->
  Writer.WriterT w (BetterEffReader h es) a
toWriter b = Writer.WriterT $ do
  MkBetterEffReader $ \hs -> do
    runWriter $ \w -> do
      bar b w hs

toReader ::
  (Handle h, Monoid w) =>
  BetterEffReader (Product (Reader w) h) es a ->
  Reader.ReaderT w (BetterEffReader h es) a
toReader b = Reader.ReaderT $ \r -> do
  MkBetterEffReader $ \hs -> do
    runReader r $ \rd -> do
      bar b rd hs

fooM ::
  (e1 :> es, e2 :> es) => State Int e1 -> Exception String e2 -> Eff es Bool
fooM _ _ = pure True

blah ::
  State.StateT Int (BetterEffReader (Product (Exception String) (ConstEffect ())) es) Bool
blah = toState $ do
  MkBetterEffReader $ uncurryP $ \st -> uncurryP $ \ex _ -> fooM st ex

blog ::
  State.StateT Int (Except.ExceptT String (BetterEffReader (ConstEffect ()) es)) Bool
blog = hoist toExcept blah

blogger :: State.StateT Int (Except.ExceptT String (Eff es)) Bool
blogger = hoist (hoist runConstReader) blog
-}
