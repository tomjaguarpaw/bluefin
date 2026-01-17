{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Bluefin.Internal.CloneableHandle
  ( module Bluefin.Internal.CloneableHandle,
    MonadUnliftIO (withRunInIO),
    MonadIO (liftIO),
    Generic1,
  )
where

import Bluefin.Internal hiding (b, race, w)
import Bluefin.Internal.OneWayCoercible
  ( OneWayCoercible (..),
    unsafeOneWayCoercible,
  )
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO (withRunInIO))
import Data.Coerce (coerce)
import GHC.Generics
  ( Generic1 (..),
    M1 (M1),
    Rec1 (Rec1),
    (:*:) ((:*:)),
  )
import GHC.TypeLits (ErrorMessage (Text), TypeError)

withEffToIOCloneHandle ::
  (e1 :> es, CloneableHandle h) =>
  IOE e1 ->
  -- | Handle accessible in the continuation
  h es ->
  -- | Continuation with the unlifting function in scope.
  ((forall r. (forall e. IOE e -> h e -> Eff e r) -> IO r) -> IO a) ->
  Eff es a
withEffToIOCloneHandle io h k = do
  withEffToIO_ io $ \runInIO -> do
    k $ \body -> do
      runInIO $ do
        cloneHandleClass h $ \h' -> do
          cloneHandleClass io $ \io' -> do
            body (mapHandle io') (mapHandle h')

newtype HandleCloner h1 h2 es
  = MkHandleCloner
      ( forall r.
        h1 es ->
        (forall e. h2 e -> Eff (e :& es) r) ->
        Eff es r
      )

instance (Handle h1, Handle h2) => Handle (HandleCloner h1 h2) where
  handleImpl = handleOneWayCoercible

instance
  (Handle h1, Handle h2) =>
  OneWayCoercible
    (HandleCloner h1 h2 e)
    (HandleCloner h1 h2 es)
  where
  -- FIXME: These instances for higher rank types are annoying
  oneWayCoercibleImpl = unsafeOneWayCoercible

cloneHandle2 ::
  (Handle h1, Handle h2, e1 :> es) =>
  HandleCloner h1 h2 es ->
  h1 e1 ->
  (forall e. h2 e -> Eff (e :& es) r) ->
  Eff es r
cloneHandle2 (MkHandleCloner c) h1 = c (mapHandle h1)

instance CloneableHandle IOE where
  cloneableHandleImpl = MkCloneableHandleD hcIOE

hcIOE :: HandleCloner IOE IOE e
hcIOE = MkHandleCloner $ \io k -> do
  useImplIn k (mapHandle io)

-- | Cloning a @State@ copies its contents to a new @State@.  Changes
-- to one will not effect the other.
instance CloneableHandle (State s) where
  cloneableHandleImpl = MkCloneableHandleD hcState

hcState :: HandleCloner (State s) (State s) e
hcState = MkHandleCloner $ \st k -> do
  s <- get st
  evalState s $ \st' ->
    useImplIn k (mapHandle st')

instance CloneableHandle (Exception a) where
  cloneableHandleImpl = MkCloneableHandleD hcException

hcException :: HandleCloner (Exception ex) (Exception ex) e
hcException = MkHandleCloner $ \ex k -> do
  useImplIn k (mapHandle ex)

instance CloneableHandle (Reader r) where
  cloneableHandleImpl = MkCloneableHandleD hcReader

hcReader :: HandleCloner (Reader r) (Reader r) e
hcReader = MkHandleCloner $ \(MkReader s) k -> do
  cloneHandleClass s $ \s' -> do
    useImplIn k (MkReader (mapHandle s'))

-- | Cloning a @HandleReader@ copies its contents to a new
-- @HandleReader@.  Changes to one will not effect the other.
instance (CloneableHandle h) => CloneableHandle (HandleReader h) where
  cloneableHandleImpl = MkCloneableHandleD hcHandleReader

cloneHandleClass ::
  (e1 :> es, CloneableHandle h) =>
  h e1 ->
  (forall e. h e -> Eff (e :& es) r) ->
  Eff es r
cloneHandleClass =
  cloneHandle2 (case cloneableHandleImpl of MkCloneableHandleD c' -> c')

hcHandleReader :: (CloneableHandle h) => HandleCloner (HandleReader h) (HandleReader h) e
hcHandleReader = MkHandleCloner $ \hr k -> do
  h <- askHandle hr
  cloneHandleClass h $ \h' -> do
    runHandleReader h' $ \hr' -> do
      useImplIn k (mapHandle hr')

instance
  (TypeError (Text "Coroutine cannot be cloned. Perhaps you want an STM channel?")) =>
  CloneableHandle (Coroutine a b)
  where
  cloneableHandleImpl =
    error "instance CloneableHandle (Coroutine a b) not implemented"

instance
  (TypeError (Text "Writer cannot be cloned. Perhaps you want an STM channel?")) =>
  CloneableHandle (Writer w)
  where
  cloneableHandleImpl =
    error "instance CloneableHandle (Writer a) not implemented"

newtype (h1 :~> h2) es = MkArrow (forall e. h1 e -> h2 (e :& es))

instance (Handle h1, Handle h2) => Handle (h1 :~> h2) where
  handleImpl = handleOneWayCoercible

instance
  (Handle h1, Handle h2, e :> es) =>
  OneWayCoercible
    ((h1 :~> h2) e)
    ((h1 :~> h2) es)
  where
  -- FIXME: These instances for higher rank types are annoying
  oneWayCoercibleImpl = unsafeOneWayCoercible

abstract ::
  (Handle h2) =>
  (forall e. h1 e -> h2 (e :& es)) ->
  -- | ͘
  (h1 :~> h2) es
abstract k = MkArrow (mapHandle . k)

app ::
  (Handle h2) =>
  (h1 :~> h2) e ->
  h1 e ->
  -- | ͘
  h2 e
app (MkArrow f) h1 = makeOpHandle (f h1)
  where
    makeOpHandle :: forall h e. (Handle h) => h (e :& e) -> h e
    makeOpHandle = case have @(e :& e) @e (subsume1 (eq ZW)) of
      Dict -> mapHandle

lmapHC ::
  (Handle h, Handle h2) =>
  (h1 :~> h2) e ->
  HandleCloner h2 h e ->
  HandleCloner h1 h e
lmapHC f hc = MkHandleCloner $ \h1 k1 -> cloneHandle2 hc (app f h1) k1

pureHC :: h2 e -> HandleCloner h1 h2 e
pureHC h2 = MkHandleCloner $ \_ k -> makeOp (k h2)

fmapHC ::
  (Handle h, Handle h1, Handle h2) =>
  (h1 :~> h2) e ->
  HandleCloner h h1 e ->
  HandleCloner h h2 e
fmapHC f h = pureHC f `apHC` h

apHC ::
  (Handle h, Handle h1, Handle h2) =>
  HandleCloner h (h1 :~> h2) e ->
  HandleCloner h h1 e ->
  HandleCloner h h2 e
apHC cf c1 = MkHandleCloner $ \h k2 -> do
  cloneHandle2 (mapHandle cf) h $ \hf -> do
    cloneHandle2 (mapHandle c1) h $ \h1 -> do
      useImplIn k2 (app (mapHandle hf) (mapHandle h1))

liftHC2 ::
  forall h h1 h2 hr es.
  (Handle h, Handle h1, Handle h2) =>
  (forall e. h1 e -> h2 e -> hr e) ->
  HandleCloner h h1 es ->
  HandleCloner h h2 es ->
  HandleCloner h hr es
liftHC2 f c1 c2 = MkHandleCloner $ \h kr -> do
  cloneHandle2 c1 h $ \h1 -> do
    cloneHandle2 (mapHandle c2) h $ \h2 -> do
      useImplIn kr (f (mapHandle h1) (mapHandle h2))

infixr 9 :~>

infixl 9 `apHC`

-- | You can use @DerivingVia@ with @GenericCloneableHandle@ to derive
-- a 'CloneableHandle' instance for your type, as long as it is a
-- product type of @CloneableHandle@s.
newtype GenericCloneableHandle h e = MkGenericCloneableHandle (h e)

instance (Handle h) => Handle (GenericCloneableHandle h) where
  handleImpl = handleMapHandle $ \(MkGenericCloneableHandle h) ->
    MkGenericCloneableHandle (mapHandle h)

instance
  (Handle h, Generic1 h, GCloneableHandle (Rep1 h)) =>
  CloneableHandle (GenericCloneableHandle h)
  where
  cloneableHandleImpl = coerce (gCloneableHandle @h)

newtype CloneableHandleD h = MkCloneableHandleD (forall e. HandleCloner h h e)

getHandleCloner :: (GCloneableHandle h) => HandleCloner h h e
getHandleCloner = case gCloneableHandleImpl of MkCloneableHandleD c -> c

gCloneableHandle ::
  forall h.
  (Handle h, Generic1 h) =>
  (GCloneableHandle (Rep1 h)) =>
  CloneableHandleD h
gCloneableHandle =
  MkCloneableHandleD
    ( fmapHC
        (abstract (mapHandle . to1))
        (lmapHC (abstract (mapHandle . from1)) getHandleCloner)
    )

-- | You can define a @CloneableHandle@ instance for your @Handle@ as
-- long as it is a product type of @CloneableHandle@s.  To define the
-- instance, use @DerivingVia@ and 'GenericCloneableHandle'.  For
-- example:
--
-- @
-- data MyHandle e = MkMyHandle ('Bluefin.Exception.Exception' String e) ('Bluefin.State.State' Int e)
--   deriving ('Bluefin.Compound.Generic', 'Generic1')
--   deriving ('Bluefin.Compound.Handle') via t'Bluefin.Compound.OneWayCoercibleHandle' MyHandle
--   deriving ('CloneableHandle') via 'GenericCloneableHandle' MyHandle
--
-- instance (e t'Bluefin.Eff.:>' es) => t'Bluefin.Compound.OneWayCoercible' (MyHandle e) (MyHandle es) where
--   'Bluefin.Compound.oneWayCoercibleImpl' = 'Bluefin.Compound.gOneWayCoercible'
-- @
class (Handle h) => CloneableHandle h where
  -- | Blah
  cloneableHandleImpl :: CloneableHandleD h

-- | @Generic@ implementation detail of
-- 'GenericCloneableHandle'. Bluefin users should never need to use
-- this directly.
class (Handle h) => GCloneableHandle h where
  gCloneableHandleImpl :: CloneableHandleD h

-- | A cloneable handle is generically cloneable
instance
  (CloneableHandle h) =>
  GCloneableHandle (Rec1 h)
  where
  gCloneableHandleImpl = coerce (cloneableHandleImpl @h)

-- | An annotated cloneable handle is generically cloneable
instance
  (GCloneableHandle h) =>
  GCloneableHandle (M1 i t h)
  where
  gCloneableHandleImpl = coerce (gCloneableHandleImpl @h)

-- | A pair of cloneable handles is generically cloneable
instance
  (GCloneableHandle h1, GCloneableHandle h2) =>
  GCloneableHandle (h1 :*: h2)
  where
  gCloneableHandleImpl =
    MkCloneableHandleD $
      (abstract $ \h1 -> abstract $ \h2 -> mapHandle h1 :*: mapHandle h2)
        `fmapHC` lmapHC (abstract $ \(h1 :*: _) -> mapHandle h1) getHandleCloner
        `apHC` lmapHC (abstract $ \(_ :*: h2) -> mapHandle h2) getHandleCloner
