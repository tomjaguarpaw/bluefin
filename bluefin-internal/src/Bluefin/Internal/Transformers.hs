module Bluefin.Internal.Transformers where

import Bluefin.Internal hiding (w)
import Control.Monad.Morph (hoist)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer

toState ::
  (forall e. State s e -> Reader.ReaderT r (Eff (e :& es)) a) ->
  State.StateT s (Reader.ReaderT r (Eff es)) a
toState k = State.StateT $ \s -> do
  Reader.ReaderT $ \r -> do
    runState s $ \st -> do
      Reader.runReaderT (k st) r

toExcept ::
  (forall e. Exception ex e -> Reader.ReaderT r (Eff (e :& es)) a) ->
  Except.ExceptT ex (Reader.ReaderT r (Eff es)) a
toExcept k = Except.ExceptT $ do
  Reader.ReaderT $ \r -> do
    try $ \ex -> do
      Reader.runReaderT (k ex) r

toWriter ::
  (Monoid s) =>
  (forall e. Writer s e -> Reader.ReaderT r (Eff (e :& es)) a) ->
  Writer.WriterT s (Reader.ReaderT r (Eff es)) a
toWriter k = Writer.WriterT $ do
  Reader.ReaderT $ \r -> do
    runWriter $ \w -> do
      Reader.runReaderT (k w) r

foo ::
  (e1 :> es, e2 :> es) => State Int e1 -> Exception String e2 -> Eff es Bool
foo _ _ = pure True

blah ::
  (e1 :> es) =>
  State.StateT Int (Reader.ReaderT (Exception String e1) (Eff es)) Bool
blah = toState $ \st -> do
  Reader.ReaderT $ \ex -> do
    foo st ex

blog ::
  State.StateT Int (Except.ExceptT String (Reader.ReaderT () (Eff es))) Bool
blog =
  hoist
    f
    blah

f ::
  (forall e. Reader.ReaderT (Exception ex e) (Eff (e :& es)) a) ->
  Except.ExceptT ex (Reader.ReaderT () (Eff es)) a
f =
  ( \reader -> toExcept $ \ex -> do
      Reader.ReaderT $ \() -> do
        Reader.runReaderT reader ex
  )

blag :: State.StateT Int (Except.ExceptT String (Eff es)) Bool
blag =
  hoist
    ( \x ->
        hoist
          (\y -> Reader.runReaderT y ())
          x
    )
    blog
