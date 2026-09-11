{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Bluefin.Internal.Prim where

import Bluefin.Internal
  ( Eff,
    Effects,
    Handle,
    OneWayCoercibleHandle,
    effIO,
    makeOp,
    unsafeProvideIO,
    (:&),
    type (<:),
  )
import Bluefin.Internal.OneWayCoercible
  ( OneWayCoercible (..),
    unsafeOneWayCoercible,
  )
import Control.Monad.Primitive qualified as P
import GHC.Exts (State#)
import Unsafe.Coerce (unsafeCoerce)

data Prim (e :: Effects) = UnsafeMkPrim
  deriving (Handle) via OneWayCoercibleHandle Prim

data PrimStateEff (es :: Effects)

instance (e <: es) => OneWayCoercible (Prim e) (Prim es) where
  oneWayCoercibleImpl = unsafeOneWayCoercible

runPrim ::
  (forall e. Prim e -> Eff (e :& es) r) ->
  -- | ͘
  Eff es r
runPrim k = makeOp (k UnsafeMkPrim)

type StateM s a = State# s -> (# State# s, a #)

unsafeCoerceStateM :: forall s1 s2 a. StateM s1 a -> StateM s2 a
unsafeCoerceStateM = unsafeCoerce

primitive ::
  forall e1 es a.
  (e1 <: es) =>
  Prim e1 ->
  (State# (PrimStateEff e1) -> (# State# (PrimStateEff e1), a #)) ->
  -- | ͘
  Eff es a
primitive UnsafeMkPrim k = unsafeProvideIO $ \io ->
  effIO
    io
    (P.primitive @IO (unsafeCoerceStateM @(PrimStateEff e1) @P.RealWorld k))
