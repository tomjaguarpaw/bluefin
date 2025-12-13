module Bluefin.Internal.FunctorCoroutine where

import Bluefin.Internal
  ( Eff,
    Effects,
    Handle (mapHandle),
    HandleD,
    HandleReader,
    handleMapHandle,
    localHandle,
    useImplIn,
    useImplUnder,
    (:&),
    (:>),
  )
import Data.Kind (Type)

type Send :: ((Type -> Type) -> Type -> Type) -> Effects -> Type
newtype Send f e = MkSend (EffectHandler f e)

instance Handle (Send f) where
  mapHandle (MkSend g) = MkSend (useImplUnder . g)

send ::
  (e1 :> es) =>
  Send f e1 ->
  f (Eff es) r ->
  -- | ͘
  Eff es r
send (MkSend g) = useImplIn g

type EffectHandler f es =
  forall e r1.
  f (Eff e) r1 ->
  -- | ͘
  Eff (e :& es) r1

interpret ::
  EffectHandler f es ->
  (forall e. Send f e -> Eff (e :& es) r) ->
  -- | ͘
  Eff es r
interpret g k = useImplIn k (MkSend g)

type Flip :: ((Type -> Type) -> k -> Type) -> k -> Effects -> Type
newtype Flip f a e = MkFlip {unFlip :: f (Eff e) a}

mapFlip ::
  (f1 (Eff e1) r1 -> f2 (Eff e2) r2) ->
  Flip f1 r1 e1 ->
  Flip f2 r2 e2
mapFlip f = MkFlip . f . unFlip

handleFlip ::
  (forall e es. (e :> es) => f (Eff e) r -> f (Eff es) r) ->
  -- | ͘
  HandleD (Flip f r)
handleFlip f = handleMapHandle (mapFlip f)

passthrough ::
  (Handle (Flip f r), e1 :> es, e2 :> es) =>
  Send f e1 ->
  f (Eff e2) r ->
  -- | ͘
  Eff es r
passthrough fc = send fc . unFlip . mapHandle . MkFlip

interpose ::
  (e1 :> es) =>
  (Send f es -> EffectHandler f es) ->
  HandleReader (Send f) e1 ->
  Eff es r ->
  -- | ͘
  Eff es r
interpose h hr = localHandle hr (\fcOrig -> MkSend (h fcOrig))
