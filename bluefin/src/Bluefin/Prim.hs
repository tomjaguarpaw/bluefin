-- | For defining @PrimMonad@ instances, for example:
--
-- @
-- -- Define a handle which includes Prim
-- data ExAndPrim e = MkExAndPrim (Exception String e) (Prim e)
--   -- Give it a Handle instance, as per Bluefin.Compound
--   deriving (Handle) via OneWayCoercibleHandle ExAndPrim
--   deriving stock (Generic)
--
-- instance (e :> es) => OneWayCoercible (ExAndPrim e) (ExAndPrim es) where
--   oneWayCoercibleImpl = gOneWayCoercible
--
-- -- Define a monad M containing the Prim handle
-- newtype M es a = MkM (DslBuilderEff ExAndPrim es a)
--   deriving newtype (Functor, Applicative, Monad)
--
-- -- Give M a PrimMonad instance
-- instance PrimMonad (M es) where
--   type PrimState (M es) = PrimStateEff es
--   primitive f =
--     MkM (dslBuilderEff (\\(MkExAndPrim _ prim) -> P.primitive prim f))
-- @
module Bluefin.Prim (Prim, PrimStateEff, primitive) where

import Bluefin.Internal.Prim
