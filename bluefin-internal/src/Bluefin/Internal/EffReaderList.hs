{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Bluefin.Internal.EffReaderList where

import Bluefin.Internal
  ( Dict (Dict),
    Eff,
    Effects,
    Handle,
    In,
    assoc1Eff,
    assoc2,
    bimap,
    has,
    have,
    makeOp,
    mapHandle,
    subsume2,
    useImpl,
    weakenEff,
    withBase,
    (:&),
    (:>),
  )
import Control.Monad (ap)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

type EffReaderListF :: [Effects -> Type] -> Effects -> Type -> Type

newtype EffReaderListArrow h hs es r
  = MkEffReaderListArrow
  { runEffReaderListArrow ::
      forall e.
      h e ->
      EffReaderList hs (e :& es) r
  }

instance (Finite hs) => Functor (EffReaderListArrow h hs es) where
  fmap f x = MkEffReaderListArrow $ \h ->
    fmap f (runEffReaderListArrow x h)

instance (Finite hs) => Applicative (EffReaderListArrow h hs es) where
  pure a = MkEffReaderListArrow (\_ -> pure a)
  (*>) = inefficientTailFromAp
  f <*> x = MkEffReaderListArrow $ \h ->
    runEffReaderListArrow f h <*> runEffReaderListArrow x h

instance (Finite hs) => Monad (EffReaderListArrow h hs es) where
  m >>= f = MkEffReaderListArrow $ \h ->
    runEffReaderListArrow m h >>= \a ->
      runEffReaderListArrow (f a) h

mapEffReaderListArrow ::
  (e :> es) =>
  EffReaderListArrow h hs e r ->
  EffReaderListArrow h hs es r
-- FIXME: deal with unsafeCoerce
mapEffReaderListArrow = unsafeCoerce

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
      ( forall e.
        (forall a es'. EffReaderList hs es' a -> Eff (e :& es') a) ->
        Eff (e :& es) b
      ) ->
      EffReaderList hs es b
  }

class Finite hs where
  finiteImpl :: FiniteD hs

instance Finite '[] where
  finiteImpl =
    MkFiniteD
      { pure_ = effReaderList . pure,
        bind_ = \m f -> effReaderList (runEffReaderList m >>= (runEffReaderList . f)),
        mapHandle_ = effReaderList . useImpl . runEffReaderList,
        withRunInEff_ =
          \toRun -> effReaderList (makeOp (toRun (useImpl . runEffReaderList)))
      }

instance (Finite hs) => Finite (h : hs) where
  finiteImpl =
    MkFiniteD
      { pure_ = MkEffReaderList . pure,
        bind_ = \m f ->
          MkEffReaderList $
            runEffReaderList_ m >>= \a -> runEffReaderList_ (f a),
        mapHandle_ =
          MkEffReaderList . mapEffReaderListArrow . runEffReaderList_,
        withRunInEff_ = \toRun -> do
          abstract $ \(h :: h e) -> do
            withRunInEff_ finiteImpl $ \runInEff ->
              assoc1Eff $ toRun $ \m -> do
                weakenEff (withBase assoc2) $ do
                  runInEff $
                    apply (mapEffReaderListEffect m) h
      }

instance (Finite hs) => Functor (EffReaderList hs es) where
  -- FIXME: Use a more efficient implementation. Will probably have to
  -- put it in Finite.
  fmap = fmapFromMonad

fmapFromMonad :: (Monad m) => (a -> b) -> m a -> m b
fmapFromMonad f m = (pure . f) =<< m

instance (Finite hs) => Applicative (EffReaderList hs es) where
  pure = pure_ finiteImpl

  (*>) = inefficientTailFromAp
  (<*>) = ap

inefficientTailFromAp :: (Applicative f) => f a -> f b -> f b
inefficientTailFromAp m1 m2 = (id <$ m1) <*> m2

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

abstract ::
  -- Finite is a redundant constraint, but it seems prudent to add it
  -- in case it is needed in the future if we change representation.
  (Finite hs) =>
  (forall e. h e -> EffReaderList hs (e :& es) r) ->
  -- | ͘
  EffReaderList (h : hs) es r
-- TODO: maybe this should be unsafeCoerce for efficiency
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
  (forall j. (forall a es'. EffReaderList hs es' a -> Eff (j :& es') a) -> Eff (j :& es) b) ->
  EffReaderList hs es b
withRunInEff = withRunInEff_ finiteImpl

newtype InEffRunner hs e
  = MkInEffRunner (forall a es'. EffReaderList hs es' a -> Eff (e :& es') a)

instance Handle (InEffRunner hs) where
  mapHandle (MkInEffRunner f) =
    MkInEffRunner (\m -> weakenEff (bimap has has) (f m))

runInEff' ::
  (e :> es) =>
  InEffRunner hs e ->
  EffReaderList hs es r ->
  Eff es r
runInEff' ier m = do
  let MkInEffRunner f = mapHandle ier
  makeOp (f m)

withRunInEff' ::
  (Finite hs) =>
  (forall e. InEffRunner hs e -> Eff (e :& es) b) ->
  EffReaderList hs es b
withRunInEff' k = withRunInEff (\runInEff -> k (MkInEffRunner runInEff))

liftEff :: (Finite hs) => Eff es b -> EffReaderList hs es b
liftEff m = withRunInEff (\_ -> useImpl m)
