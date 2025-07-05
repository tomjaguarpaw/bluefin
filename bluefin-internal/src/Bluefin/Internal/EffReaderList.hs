{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Bluefin.Internal.EffReaderList where

import Bluefin.Internal
  ( Dict (Dict),
    Eff,
    Effects,
    In,
    bimap,
    has,
    have,
    subsume2,
    useImpl,
    (:&),
    (:>), makeOp, assoc1Eff,
  )
import Control.Monad (ap)
import Data.Coerce (coerce)
import Data.Kind (Type)

type EffReaderListF :: [Effects -> Type] -> Effects -> Type -> Type

newtype EffReaderListArrow h hs es r
  = MkEffReaderListArrow
  { runEffReaderListArrow ::
      forall e.
      h e ->
      EffReaderList hs (e :& es) r
  }

newtype EffReaderListHandle hs r es
  = MkEffReaderListHandle {unEffReaderListHandle :: EffReaderList hs es r}

data FiniteD hs = MkFiniteD
  { pure_ :: forall es r. r -> EffReaderList hs es r,
    bind_ ::
      forall es a b.
      EffReaderList hs es a ->
      (a -> EffReaderList hs es b) ->
      EffReaderList hs es b,
    mapHandle_ ::
      forall e es r.
      (e :> es) =>
      EffReaderList hs e r ->
      EffReaderList hs es r,
    withRunInEff_ ::
      forall es b.
      (forall e. (forall a. EffReaderList hs es a -> Eff (e :& es) a) -> Eff (e :& es) b) ->
      EffReaderList hs es b
  }

class Finite hs where
  finiteImpl :: FiniteD hs

instance Finite '[] where
  finiteImpl =
    MkFiniteD
      { pure_ = effReaderList . pure,
        bind_ = (>>=),
        mapHandle_ = effReaderList . useImpl . runEffReaderList,
        withRunInEff_ =
          \toRun -> effReaderList (makeOp (toRun (useImpl . runEffReaderList)))
      }

instance (Finite hs) => Finite (h : hs) where
  finiteImpl =
    MkFiniteD
      { pure_ = \r -> abstract $ \_ -> pure r,
        bind_ = \m f ->
          abstract $ \r -> apply' m r >>= \a -> apply' (f a) r,
        mapHandle_ = \e -> abstract $ \h -> apply' e h,
        withRunInEff_ = \toRun ->
          abstract $ \(h :: h e) ->
            withRunInEff $ \runInEff ->
              assoc1Eff $ toRun $ \m -> do
                assoc2Eff $ runInEff $ apply (mapEffReaderListEffect m) h
      }

assoc2Eff :: Eff (e1 :& (e2 :& es)) r -> Eff ((e1 :& e2) :& es) r
assoc2Eff = undefined

instance (Finite hs) => Functor (EffReaderList hs es) where
  -- FIXME: use a more efficient implementation
  fmap = fmapFromMonad

fmapFromMonad :: (Monad m) => (a -> b) -> m a -> m b
fmapFromMonad f m = (pure . f) =<< m

instance (Finite hs) => Applicative (EffReaderList hs es) where
  pure = pure_ finiteImpl

  -- FIXME: use a more efficient implementation
  m1 *> m2 = (id <$ m1) <*> m2
  (<*>) = ap

instance (Finite hs) => Monad (EffReaderList hs es) where
  (>>=) = bind_ finiteImpl

type family EffReaderListF l es r = e | e -> l es r where
  EffReaderListF '[] es r = Eff es r
  EffReaderListF (h : hs) es r = EffReaderListArrow h hs es r

newtype EffReaderList hs es r
  = MkEffReaderList {runEffReaderList_ :: EffReaderListF hs es r}

mapEffReaderListEffectIn ::
  forall es e l r.
  (Finite l) =>
  e `In` es ->
  EffReaderList l e r ->
  EffReaderList l es r
mapEffReaderListEffectIn in_ m =
  case have in_ of Dict -> mapHandle_ finiteImpl m

mapEffReaderListEffect ::
  (e :> es, Finite l) =>
  EffReaderList l e r ->
  EffReaderList l es r
mapEffReaderListEffect = mapEffReaderListEffectIn has

mapEffReaderListEffectUnder ::
  forall es e e1 l r.
  (e :> es, Finite l) =>
  EffReaderList l (e1 :& e) r ->
  EffReaderList l (e1 :& es) r
mapEffReaderListEffectUnder = mapEffReaderListEffectIn (bimap has has)

apply ::
  (e :> es, Finite hs) =>
  EffReaderList (h : hs) es r ->
  h e ->
  -- | ͘
  EffReaderList hs es r
apply (MkEffReaderList e) h =
  mapEffReaderListEffectIn (subsume2 has) (runEffReaderListArrow e h)

apply' ::
  (e1 :> es, e2 :> es, Finite hs) =>
  EffReaderList (h : hs) e1 r ->
  h e2 ->
  EffReaderList hs es r
apply' = apply . mapEffReaderListEffect

abstract ::
  -- Finite is a redundant constraint, but it seems prudent to add it
  -- in case it is needed in the future if we change representation.
  (Finite hs) =>
  (forall e. h e -> EffReaderList hs (e :& es) r) ->
  -- | ͘
  EffReaderList (h : hs) es r
abstract f = coerce (MkEffReaderListArrow f)

effReaderList ::
  Eff es r ->
  -- | ͘
  EffReaderList '[] es r
effReaderList = coerce

runEffReaderList ::
  EffReaderList '[] es r ->
  -- | ͘
  Eff es r
runEffReaderList = coerce

withRunInEff ::
  (Finite hs) =>
  (forall e. (forall a. EffReaderList hs es a -> Eff (e :& es) a) -> Eff (e :& es) b) ->
  EffReaderList hs es b
withRunInEff = withRunInEff_ finiteImpl
