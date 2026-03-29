{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Bluefin.Internal.Prim where

import Bluefin.Internal
import Bluefin.Internal.OneWayCoercible
import Control.Monad.Primitive (RealWorld)
import Control.Monad.Primitive qualified as P
import GHC.Exts (State#)
import Unsafe.Coerce (unsafeCoerce)

data Prim (e :: Effects) = UnsafeMkPrim
  deriving (Handle) via OneWayCoercibleHandle Prim

data PrimStateEff (es :: Effects)

instance (e :> es) => OneWayCoercible (Prim e) (Prim es) where
  oneWayCoercibleImpl = unsafeOneWayCoercible

runPrim ::
  (forall e. Prim e -> Eff (e :& es) r) ->
  -- | ͘
  Eff es r
runPrim k = makeOp (k UnsafeMkPrim)

type StateM s a = (State# s -> (# State# s, a #))

type StateMToIO s a = StateM s a -> IO a

primitive ::
  forall es e1 a.
  (e1 :> es) =>
  Prim e1 ->
  (State# (PrimStateEff e1) -> (# State# (PrimStateEff e1), a #)) ->
  -- | ͘
  Eff es a
primitive UnsafeMkPrim k = unsafeProvideIO $ \io ->
  effIO
    io
    ( unsafeCoerce
        @(StateMToIO RealWorld a)
        @(StateMToIO (PrimStateEff e1) a)
        (P.primitive @IO)
        k
    )
