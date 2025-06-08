{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Bluefin.Internal.Transformers where

import Bluefin.Internal hiding (w)
-- import Control.Monad.Morph (hoist)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import Data.Coerce (coerce)
import Data.Kind (Type)

toState ::
  Handle h =>
  BetterEffReader (Product (State s) h) es a ->
  State.StateT s (BetterEffReader h es) a
toState b = State.StateT $ \s -> do
  MkBetterEffReader $ \hs -> do
    runState s $ \st -> do
      case b of
        MkBetterEffReader k -> do
          curryP (useImplUnder . k) st hs

{-
\r -> do
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

fooM ::
  (e1 :> es, e2 :> es) => State Int e1 -> Exception String e2 -> Eff es Bool
fooM _ _ = pure True

blah ::
  (e1 :> es) =>
  State.StateT Int (Reader.ReaderT (Exception String e1) (Eff es)) Bool
blah = toState $ \st -> do
  Reader.ReaderT $ \ex -> do
    fooM st ex

blog ::
  State.StateT Int (Except.ExceptT String (Reader.ReaderT () (Eff es))) Bool
blog =
  hoist
    _
    blah

ff ::
  (forall e. Reader.ReaderT (Exception ex e) (Eff (e :& es)) a) ->
  Except.ExceptT ex (Reader.ReaderT () (Eff es)) a
ff =
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

--

newtype (h :~> t) es = Nest {unNest :: forall e. h e -> t (e :& es)}

instance (Handle h, Handle t) => Handle (h :~> t) where
  mapHandle (Nest f) = Nest (\h -> mapHandleWith (bimap has has) (f h))

newtype Forall t = Forall {unForall :: forall es. t es}

newtype WrapEff r es = MkWrapEff {unWrapEff :: Eff es r}

instance Handle (WrapEff r) where
  mapHandle (MkWrapEff e) = MkWrapEff (useImpl e)

type F :: [Effects -> Type] -> (Effects -> Type) -> Effects -> Type
type family F ts h where
  F '[] h = h
  F (h1 : hs) h = (h1 :~> F hs h)

type M :: [Effects -> Type] -> (Effects -> Type) -> Effects -> Type
newtype M ts h es = MkM (F ts h es)

applyM :: M (h : hs) b es -> h e -> M hs b (e :& es)
applyM (MkM (Nest f)) h = MkM (f h)

abstractM :: (forall e. h e -> M hs b (e :& es)) -> M (h : hs) b es
abstractM k = MkM (Nest (\h -> case k h of MkM m -> m))

liftM0 :: (Handle h, e :> es) => h e -> M '[] h es
liftM0 h = MkM (mapHandle h)

type N :: [Effects -> Type] -> Effects -> Type -> Type
newtype N ts es a = MkN (M ts (WrapEff a) es)

instance Functor (N '[] es) where
  fmap = foo0

foo0 :: forall es a b. (a -> b) -> N '[] es a -> N '[] es b
foo0 = coerce (fmap @(Eff es) @a @b)

instance (forall es'. Functor (N hs es')) => Functor (N (h1 : hs) es) where
  fmap = foo fmap

instance Applicative (N '[] es) where
  pure a = MkN (MkM ((MkWrapEff (pure a))))
  MkN (MkM (MkWrapEff f)) <*> MkN (MkM (MkWrapEff x)) =
    MkN (MkM (MkWrapEff (f <*> x)))

instance (forall es'. Applicative (N hs es')) => Applicative (N (h1 : hs) es) where
  pure a = MkN $ abstractM $ \_ ->
    case pure a of MkN n -> n

  MkN f <*> MkN x = MkN $ abstractM $ \h ->
    case MkN (applyM f h) <*> MkN (applyM x h) of MkN r -> r

instance (forall es'. Monad (N hs es')) => Monad (N (h1 : hs) es) where
  MkN m >>= f = MkN $ abstractM $ \h ->
    case MkN (applyM m h) >>= (\a -> MkN (applyM (case f a of MkN r -> r) h)) of MkN r -> r

foo ::
  (forall es' a b. (a -> b) -> (N hs es' a -> N hs es' b)) ->
  (forall es' a b. (a -> b) -> (N (h : hs) es' a -> N (h : hs) es' b))
foo fmap1 f (MkN m) =
  MkN
    ( abstractM $ \h ->
        case fmap1 f (MkN (applyM m h)) of MkN n -> n
    )

instance (Handle h) => Handle (M '[] h) where
  mapHandle (MkM h) = MkM (mapHandle h)

instance (Handle h, Handle (M hs h)) => Handle (M (h : hs) h) where
  mapHandle m = abstractM $ \h ->
    mapHandleWith (bimap has has) (applyM m h)

apply :: (e :> es, Handle (M hs b)) => h e -> M (h : hs) b es -> M hs b es
apply h m = handleBoth (mapHandleWith (bimap has has) (applyM m h))

handleBoth :: (Handle h) => h (e :& e) -> h e
handleBoth = mapHandleWith (subsume1 has)
-}
