{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Bluefin.Internal.Transformers where

import Bluefin.Internal hiding (b, w)
import Control.Monad.Morph (hoist)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import Data.Coerce (coerce)
import Data.Kind (Type)

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

toState ::
  (Handle h) =>
  BetterEffReader (Product (State s) h) es a ->
  State.StateT s (BetterEffReader h es) a
toState b = State.StateT $ \s -> do
  MkBetterEffReader $ \hs -> do
    runState s $ \st -> do
      bar b st hs

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
