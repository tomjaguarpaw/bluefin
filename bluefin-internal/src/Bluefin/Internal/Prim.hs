{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DerivingVia #-}

module Bluefin.Internal.Prim where

import Bluefin.Internal
import Bluefin.Internal.OneWayCoercible
import Control.Monad.Primitive qualified as P
import GHC.Exts (State#)
import Unsafe.Coerce (unsafeCoerce)

data Prim (e :: Effects) = UnsafeMkPrim
  deriving Handle via OneWayCoercibleHandle Prim

data PrimStateEff (es :: Effects)

instance (e :> es) => OneWayCoercible (Prim e) (Prim es) where
  oneWayCoercibleImpl = unsafeOneWayCoercible

primitive ::
  (e1 :> es, e2 :> es) =>
  Prim e1 ->
  (State# (PrimStateEff e2) -> (# State# (PrimStateEff e2), a #)) ->
  -- | ͘
  Eff es a
primitive UnsafeMkPrim = unsafeCoerce (P.primitive @IO)
