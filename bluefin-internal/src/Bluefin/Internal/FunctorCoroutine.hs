module Bluefin.Internal.FunctorCoroutine where

import Bluefin.Internal
  ( Eff,
    Handle (mapHandle),
    useImpl,
    useImplIn,
    useImplUnder,
    (:&),
    (:>),
  )

newtype Send f e
  = MkSend (forall e' r. f (Eff e') r -> Eff (e' :& e) r)

instance Handle (Send f) where
  mapHandle (MkSend g) =
    MkSend (useImplUnder . g)

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

class MFunctor f where
  mfmap ::
    (forall r. m1 r -> m2 r) ->
    f m1 a ->
    -- | ͘
    f m2 a

passthrough ::
  (MFunctor f, e1 :> es, e2 :> es) =>
  Send f e1 ->
  f (Eff e2) r ->
  -- | ͘
  Eff es r
passthrough fc = send fc . mfmap useImpl
