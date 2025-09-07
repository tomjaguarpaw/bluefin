{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Bluefin.Internal.EffReaderList where

import Bluefin.Internal
  ( Dict (Dict),
    Eff,
    Effects,
    Handle,
    HandleReader,
    In,
    State,
    bimap,
    has,
    have,
    makeOp,
    mapHandle,
    sndI,
    subsume2,
    useImpl,
    useImplIn,
    weakenEff,
    withBase,
    (:&),
    (:>), evalState, HandleReader (UnsafeMkHandleReader),
  )
import qualified Bluefin.Internal as B
import Control.Monad (ap)
import Control.Monad.RWS.Class (MonadState (put))
import Control.Monad.State (MonadState (get))
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Type.Coercion (Coercion (Coercion))
import Unsafe.Coerce (unsafeCoerce)

type EffReaderListF :: [Effects -> Type] -> Effects -> Type -> Type

newtype EffArrow h eff es r
  = MkEffArrow (forall e. h e -> eff (e :& es) r)

newtype EffReaderListArrow h hs es r
  = MkEffReaderListArrow (EffArrow h (EffReaderList hs) es r)

runEffReaderListArrow ::
  EffReaderListArrow h hs es r ->
  h e ->
  EffReaderList hs (e :& es) r
runEffReaderListArrow (MkEffReaderListArrow (MkEffArrow k)) = k

mkEffReaderListArrow ::
  (forall e. h e -> EffReaderList hs (e :& es) r) ->
  EffReaderListArrow h hs es r
mkEffReaderListArrow k = MkEffReaderListArrow (MkEffArrow k)

instance (Finite hs) => Functor (EffReaderListArrow h hs es) where
  fmap f x = mkEffReaderListArrow $ \h ->
    fmap f (runEffReaderListArrow x h)

instance (Finite hs) => Applicative (EffReaderListArrow h hs es) where
  pure a = mkEffReaderListArrow (\_ -> pure a)
  (*>) = inefficientTailFromAp
  f <*> x = mkEffReaderListArrow $ \h ->
    runEffReaderListArrow f h <*> runEffReaderListArrow x h

instance (Finite hs) => Monad (EffReaderListArrow h hs es) where
  m >>= f = mkEffReaderListArrow $ \h ->
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
  { pure_ ::
      forall es r.
      r ->
      EffReaderList hs es r,
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
        InEffRunner hs e ->
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
        bind_ = \m f ->
          effReaderList (runEffReaderList m >>= (runEffReaderList . f)),
        mapHandle_ = effReaderList . useImpl . runEffReaderList,
        withRunInEff_ = \toRun ->
          effReaderList
            (makeOp (toRun (MkInEffRunner (useImpl . runEffReaderList))))
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
            withRunInEff_ finiteImpl $ \rie ->
              useImplIn toRun $ MkInEffRunner $ \m -> do
                runInEff rie $
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

apply0 ::
  (e :> es, es' :> es, Finite hs) =>
  EffReaderList (h : hs) es' r ->
  h e ->
  -- | ͘
  EffReaderList hs es r
apply0 e = apply (mapEffReaderListEffect e)

apply ::
  (e :> es, Finite hs) =>
  EffReaderList (h : hs) es r ->
  h e ->
  -- | ͘
  EffReaderList hs es r
apply (MkEffReaderList e) h =
  mapEffReaderListEffectIn (subsume2 has) (runEffReaderListArrow e h)

apply1 ::
  EffReaderList (h : '[]) es r ->
  h es ->
  -- | ͘
  Eff es r
apply1 e h = runEffReaderList (apply (mapEffReaderListEffect e) h)

abstract ::
  -- Finite is a redundant constraint, but it seems prudent to add it
  -- in case it is needed in the future if we change representation.
  (Finite hs) =>
  (forall e. h e -> EffReaderList hs (e :& es) r) ->
  -- | ͘
  EffReaderList (h : hs) es r
-- TODO: maybe this should be unsafeCoerce for efficiency
abstract = abstractNoFinite

abstractNoFinite ::
  (forall e. h e -> EffReaderList hs (e :& es) r) ->
  -- | ͘
  EffReaderList (h : hs) es r
-- TODO: maybe this should be unsafeCoerce for efficiency
abstractNoFinite f = coerce (mkEffReaderListArrow f)

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

newtype InEffRunner hs e
  = MkInEffRunner (forall a e'. EffReaderList hs e' a -> Eff (e' :& e) a)

instance Handle (InEffRunner hs) where
  mapHandle (MkInEffRunner f) =
    MkInEffRunner (\m -> weakenEff (bimap has has) (f m))

runInEff ::
  (e :> es) =>
  InEffRunner hs e ->
  EffReaderList hs es r ->
  -- | ͘
  Eff es r
runInEff ier m = do
  let MkInEffRunner f = mapHandle ier
  makeOp (f m)

withRunInEff ::
  (Finite hs) =>
  (forall e. InEffRunner hs e -> Eff (e :& es) r) ->
  -- | ͘
  EffReaderList hs es r
withRunInEff = withRunInEff_ finiteImpl

liftEff :: (Finite hs) => Eff es b -> EffReaderList hs es b
liftEff m = withRunInEff_ finiteImpl (\_ -> useImpl m)

blah ::
  ((forall e. h e -> Eff (e :& es) r) -> r1) ->
  EffReaderList '[h] es r ->
  r1
blah h erl = h $ \st ->
  runEffReaderList (apply (mapEffReaderListEffectIn (withBase sndI) erl) st)

blaz ::
  (Finite hs, e3 :> es) =>
  (forall e. EffReaderList '[h] (e :& es) r1 -> Eff (e :& es) r2) ->
  EffReaderList (h : hs) e3 r1 ->
  EffReaderList hs es r2
blaz karg b =
  withRunInEff $ \rie ->
    karg $ abstract $ \h ->
      effReaderList $
        runInEff rie $ do
          mapEffReaderListEffect b `apply` h

-- | Use an 'Eff' handler to handler an 'EffReaderList'
effReaderListHandler ::
  (Finite hs, e3 :> es) =>
  (forall e. (forall e1. h e1 -> Eff (e1 :& (e :& es)) r1) -> Eff (e :& es) r2) ->
  EffReaderList (h : hs) e3 r1 ->
  EffReaderList hs es r2
effReaderListHandler h = blaz (blah h)

newtype Membership h hs
  = MkMembership (forall a es. EffReaderList '[h] es a -> EffReaderList hs es a)

here :: (Finite hs) => Membership h (h : hs)
here = MkMembership $ \erl -> abstract $ \h -> do
  let p = apply (mapEffReaderListEffect erl) h
  liftEff (runEffReaderList p)

there :: (Finite hs) => Membership h hs -> Membership h (h' : hs)
there m = MkMembership $ \erl -> abstract $ \_ -> do
  case m of MkMembership k -> mapEffReaderListEffect (k erl)

class (Finite hs) => Member h hs where
  member :: Membership h hs

instance (Finite hs) => Member h (h : hs) where
  member = here

instance (Member h hs) => Member h (h' : hs) where
  member = there member

withMembership ::
  Membership h hs ->
  (forall e. h e -> Eff (e :& es) r) ->
  EffReaderList hs es r
withMembership (MkMembership k) m = k (abstract (\h -> effReaderList (m h)))

withMember ::
  (Member h hs) =>
  (forall e. h e -> Eff (e :& es) r) ->
  EffReaderList hs es r
withMember = withMembership member

instance
  (Member (B.State s) hs, MonadState s (EffReaderList hs es)) =>
  MonadState s (EffReaderList hs es)
  where
  put = \s -> withMember (flip B.put s)
  get = withMember B.get

example :: (Member (B.State ()) hs) => EffReaderList hs es ()
example = pure () :: (MonadState () m) => m ()

abstract1 ::
  (forall e. h e -> Eff (e :& es) r) ->
  -- | ͘
  EffReaderList '[h] es r
-- TODO: maybe this should be unsafeCoerce for efficiency
abstract1 f = abstract $ \h ->
  effReaderList (f h)
