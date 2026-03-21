{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Bluefin.Examples.Prim where

import Bluefin.Compound
  ( Generic,
    Handle,
    OneWayCoercible (..),
    OneWayCoercibleHandle (..),
    gOneWayCoercible,
  )
import Bluefin.DslBuilderEff (DslBuilderEff, dslBuilderEff)
import Bluefin.Eff ((:>))
import Bluefin.Exception (Exception)
import Bluefin.Prim (Prim, PrimStateEff)
import Bluefin.Prim qualified as P
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))

-- Define a handle which includes Prim
data ExAndPrim e = MkExAndPrim (Exception String e) (Prim e)
  -- Give it a Handle instance, as per Bluefin.Compound
  deriving (Handle) via OneWayCoercibleHandle ExAndPrim
  deriving stock (Generic)

instance (e :> es) => OneWayCoercible (ExAndPrim e) (ExAndPrim es) where
  oneWayCoercibleImpl = gOneWayCoercible

-- Define a monad M containing the Prim handle
newtype M es a = MkM (DslBuilderEff ExAndPrim es a)
  deriving newtype (Functor, Applicative, Monad)

-- Give M a PrimMonad instance
instance PrimMonad (M es) where
  type PrimState (M es) = PrimStateEff es
  primitive f =
    MkM (dslBuilderEff (\(MkExAndPrim _ prim) -> P.primitive prim f))
