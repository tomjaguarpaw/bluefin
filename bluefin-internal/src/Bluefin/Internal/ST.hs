{-# LANGUAGE RoleAnnotations #-}

module Bluefin.Internal.ST where

import Bluefin.Internal
import Bluefin.Internal.OneWayCoercible
  ( OneWayCoercible (oneWayCoercibleImpl),
    unsafeOneWayCoercible,
  )
import Control.Monad.ST (RealWorld, ST, stToIO)
import Data.Type.Equality ((:~:) (Refl))

newtype STE s (e :: Effects) = UnsafeMkSTE (s :~: RealWorld)

type role STE nominal nominal

instance (e :> es) => OneWayCoercible (STE s e) (STE s es) where
  oneWayCoercibleImpl = unsafeOneWayCoercible

runST ::
  (forall s e. STE s e -> Eff (e :& es) r) ->
  -- | ͘
  Eff es r
runST k = makeOp (k (UnsafeMkSTE Refl))

effST ::
  STE s e1 ->
  ST s r ->
  -- | ͘
  Eff es r
effST (UnsafeMkSTE Refl) = UnsafeMkEff . stToIO
