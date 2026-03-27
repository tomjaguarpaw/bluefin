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
import Bluefin.Eff (Eff, runPureEff, (:>))
import Bluefin.Exception (Exception, try)
import Bluefin.Prim qualified as P
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import Data.Primitive.Array qualified as A
import Data.Traversable (for)

-- Define a handle which includes Prim
data ExAndPrim e = MkExAndPrim (Exception String e) (P.Prim e)
  -- Give it a Handle instance, as per Bluefin.Compound
  deriving (Handle) via OneWayCoercibleHandle ExAndPrim
  deriving stock (Generic)

instance (e :> es) => OneWayCoercible (ExAndPrim e) (ExAndPrim es) where
  oneWayCoercibleImpl = gOneWayCoercible

-- Define a monad M containing the Prim handle
newtype M e es a = MkM (ReaderT (ExAndPrim e) (Eff es) a)
  deriving newtype (Functor, Applicative, Monad)

-- Define a way of running M
runM ::
  (e1 :> es, e2 :> es) =>
  Exception String e1 ->
  P.Prim e2 ->
  M es es r ->
  Eff es r
runM ex prim (MkM m) =
  runReaderT m (MkExAndPrim (mapHandle ex) (mapHandle prim))

-- Give M a PrimMonad instance
instance (e :> es) => PrimMonad (M e es) where
  type PrimState (M e es) = P.PrimStateEff e
  primitive f =
    MkM (ReaderT (\(MkExAndPrim _ prim) -> P.primitive prim f))

-- ghci> example
-- Right ["Hello","World"]
example :: Either String [String]
example = runPureEff $ try $ \ex -> P.runPrim $ \prim -> do
  runM ex prim $ do
    arr <- A.newArray 2 "Hello"
    A.writeArray arr 1 "World"
    for [0, 1] (A.readArray arr)
