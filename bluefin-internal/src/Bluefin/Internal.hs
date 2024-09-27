{-# LANGUAGE LinearTypes #-}

module Bluefin.Internal where

import qualified Unsafe.Linear
import Prelude

data Effects = Union Effects Effects

-- | @type (:&) :: Effects -> Effects -> Effects@
--
-- Union of effects
infixr 9 :&

type (:&) = Union

newtype Eff (es :: Effects) a = UnsafeMkEff {unsafeUnEff :: IO a}

-- | Handle to an exception of type @exn@
newtype Exception exn (e :: Effects) = UnsafeMkException (forall a. exn -> IO a)

-- | Effect subset constraint
class (es1 :: Effects) :> (es2 :: Effects)

throw ::
  (e :> es) =>
  Exception ex e ->
  ex ->
  Eff es a
throw (UnsafeMkException throw_) e = UnsafeMkEff (throw_ e)

throwL ::
  (e :> es) =>
  Exception ex e ->
  ex %1 ->
  Eff es a
throwL ex = Unsafe.Linear.toLinear (throw ex)
