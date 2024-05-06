{-# LANGUAGE
  KindSignatures,
  RankNTypes,
  TypeOperators #-}

module Bluefin.Internal.Handle
  ( -- * Effect signatures
    Sig
  , CovariantSig(..)

    -- * Handle usage
  , Handle

    -- * Handle creation
  , HandleImpl
  , with
  ) where

import Data.Kind (Type)
import Bluefin.Internal (Eff, mergeEff, useImpl, type (:&), type (:>))

-- | An effect signature declares a set of effectful operations.
--
-- Concretely, it should be a record of functions parameterized by a monad
-- which occurs in the codomain of every field.
--
-- A handle of signature @f@ can be defined simply as a value of type @f (Eff s)@.
-- But to use it, a handle will have type @'Handle' f@ so that its operations
-- can be invoked with different effect rows in 'Eff', as long as they contain
-- the row @s@ in the type of its original definition @f (Eff s)@.
-- See 'Handle' and 'with' for more detaills.
--
-- === Example
--
-- Effect with the two state operations @get@ and @put@:
--
-- @
-- type State :: Type -> 'Sig'
-- data State s m = MkState
--   { get :: m s
--   , put :: s -> m ()
--   }
-- @
type Sig = (Type -> Type) -> Type

-- | Effect signatures must be instances of this class: they are higher-order functors.
class CovariantSig (f :: Sig) where
  smap :: (forall x. m x -> n x) -> f m -> f n

-- | Handle of signature @f@ that can be used in an effectful computation @'Eff' s1@,
-- given a constraint @s :> s1@.
--
-- === Example
--
-- Given a handle @h@ of type @'Handle' f s0@, you can invoke its operations as
-- @get h@ and @put h@, or @h.get@ and @h.put@ by using the extension
-- @OverloadedRecordDot@.
--
-- Indeed, thanks to the polymorphism in 'Handle', the field accessors can be specialized
-- to the following types:
--
-- @
-- get :: z0 :> z => 'Handle' (State s) z0 -> 'Eff' z s
-- put :: z0 :> z => 'Handle' (State s) z0 -> s -> 'Eff' z ()
-- @
type Handle f s = (forall s1. s :> s1 => f (Eff s1))

-- | Handle implementation of signature @f@.
type HandleImpl f s = f (Eff s)

-- | Create a 'Handle' @h@ with signature @f@, using the given implementation @impl@.
--
-- @
-- 'with' impl \\h -> ...
-- @
with :: CovariantSig f =>
  HandleImpl f s ->
  (forall s0. Handle f s0 -> Eff (s0 :& s) a) ->
  Eff s a
with handle action = mergeEff (action (smap useImpl handle))
-- Internally, we just instantiate s0 with s.
