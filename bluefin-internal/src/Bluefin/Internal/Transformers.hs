{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Bluefin.Internal.Transformers where

import Bluefin.Internal hiding (b, w)
import Bluefin.Internal.EffReaderList
-- import Control.Monad.Morph (hoist)
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
  withRunInEff $ \runInEff -> do
    traceM "runState"
    runState s $ \st -> do
      traceM "weakenEff"
      weakenEff (withBase $ \base -> bimap has (swap base) `cmp` assoc2 base `cmp` bimap (swap base) has `cmp` assoc1 base) $
        runInEff $
          apply'' b st

example :: State.StateT Int (EffReaderList '[] es) ()
example = toState $ abstract $ \st -> effReaderList $ do
  n <- get st
  put st (n + 1)

exampleSmall :: State.StateT Int (EffReaderList '[] es) ()
exampleSmall = toState $ pure ()

example1 :: ((), Int)
example1 = runPureEff $ do
  traceM "runEffReaderList"
  runEffReaderList $ do
    traceM "flip"
    flip State.runStateT 42 exampleSmall

basic :: ()
basic = runPureEff $ do
  runEffReaderList $ do
    trace "pure" (pure ())
    pure ()

-- forTransformers :: In (e :& (es :& e1)) (e1 :& (e :& es))
-- forTransformers

{-
  MkBetterEffReader $ \hs -> do
    runState s $ \st -> do
      bar b st hs
-}

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
