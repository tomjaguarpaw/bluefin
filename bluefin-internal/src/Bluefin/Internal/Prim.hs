{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}

module Bluefin.Internal.Prim where

import Bluefin.Internal
  ( Eff,
    Effects,
    Handle,
    HandleD (MkHandleD),
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
import Data.Kind (Type)
import GHC.Exts (State#)
import Unsafe.Coerce (unsafeCoerce)

type Prim :: Effects -> Effects -> Type
data Prim e1 e2 = UnsafeMkPrim
  deriving (Handle) via OneWayCoercibleHandle (Prim e1)

type role Prim nominal nominal

data PrimStateEff (es :: Effects)

instance (e2 <: es) => OneWayCoercible (Prim e1 e2) (Prim e1 es) where
  oneWayCoercibleImpl = unsafeOneWayCoercible

runPrim ::
  (forall e. Prim e e -> Eff (e :& es) r) ->
  -- | ͘
  Eff es r
runPrim k = makeOp (k UnsafeMkPrim)

type StateM s a = State# s -> (# State# s, a #)

unsafeCoerceStateM :: forall s1 s2 a. StateM s1 a -> StateM s2 a
unsafeCoerceStateM = unsafeCoerce

primitive ::
  forall e1 e2 es a.
  (e2 <: es) =>
  Prim e1 e2 ->
  (State# (PrimStateEff e1) -> (# State# (PrimStateEff e1), a #)) ->
  -- | ͘
  Eff es a
primitive UnsafeMkPrim k = unsafeProvideIO $ \io ->
  effIO
    io
    (P.primitive @IO (unsafeCoerceStateM @(PrimStateEff e1) @P.RealWorld k))
