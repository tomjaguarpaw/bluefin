{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Bluefin.Examples.Prim where

import Bluefin.Compound
  ( Generic,
    Handle,
    OneWayCoercible (..),
    OneWayCoercibleHandle (..),
    gOneWayCoercible,
    mapHandle,
  )
import Bluefin.DslBuilder (DslBuilder, dslBuilder, runDslBuilder)
import Bluefin.Eff (Eff, runPureEff, type (<:))
import Bluefin.Exception (Exception, try)
import Bluefin.Prim qualified as P
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Data.Primitive.Array qualified as A
import Data.Traversable (for)

-- Define a capability which includes Prim
data ExAndPrim e1 e2 = MkExAndPrim (Exception String e2) (P.Prim e1 e2)
  -- Give it a Handle instance, as per Bluefin.Compound
  deriving (Handle) via OneWayCoercibleHandle (ExAndPrim e1)
  deriving stock (Generic)

instance (e2 <: es) => OneWayCoercible (ExAndPrim e1 e2) (ExAndPrim e1 es) where
  oneWayCoercibleImpl = gOneWayCoercible

-- Define a monad M containing the Prim handle
newtype M e a = MkM (DslBuilder (ExAndPrim e) a)
  deriving newtype (Functor, Applicative, Monad)

-- Define a way of running M
runM ::
  (e1 <: es, e2 <: es) =>
  Exception String e1 ->
  P.Prim e e2 ->
  M e r ->
  Eff es r
runM ex prim (MkM m) =
  runDslBuilder (MkExAndPrim (mapHandle ex) (mapHandle prim)) m

-- Give M a PrimMonad instance
instance PrimMonad (M e) where
  type PrimState (M e) = P.PrimStateEff e
  primitive f =
    MkM (dslBuilder (\(MkExAndPrim _ prim) -> P.primitive prim f))

-- ghci> example
-- Right ["Hello","World"]
example :: Either String [String]
example = runPureEff $ try $ \ex -> P.runPrim $ \prim -> do
  runM ex prim $ do
    arr <- A.newArray 2 "Hello"
    A.writeArray arr 1 "World"
    for [0, 1] (A.readArray arr)
