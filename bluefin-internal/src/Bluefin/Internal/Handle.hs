{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Bluefin.Internal.Handle
  ( -- * Effect signatures
    Sig,

    -- * Handle usage
    Handle,

    -- * Handle creation
    with,
  )
where

import Bluefin.Internal (Eff, Effects, IsHandle (..), mergeEff, type (:&), type (:>))
import Data.Kind (Type)

-- | An effect signature declares a set of effectful operations.
--
-- Concretely, it should be a record of functions parameterized by a monad
-- which occurs in the codomain of every field.
--
-- A handle of signature @f@ can be defined simply as a value of type @f (Eff e)@.
-- But to use it, a handle will have type @'Handle' f@ so that its operations
-- can be invoked with different effect rows in 'Eff', as long as they contain
-- the row @e@ in the type of its original definition @f (Eff e)@.
-- See 'Handle' and 'with' for more detaills.
--
-- === Example
--
-- Effect with the two state operations @get@ and @put@:
--
-- @
-- type State :: Type -> 'Sig'
-- data State s e = MkState
--   { get :: Eff e s
--   , put :: s -> Eff e ()
--   }
-- @
type Sig = Effects -> Type

-- | Handle of signature @f@ that can be used in an effectful computation @'Eff' e1@,
-- given a constraint @e :> e1@.
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
-- get :: e0 :> e => 'Handle' (State s) e0 -> 'Eff' e s
-- put :: e0 :> e => 'Handle' (State s) e0 -> s -> 'Eff' e ()
-- @
type Handle f e = (forall e1. (e :> e1) => f e1)

-- | Create a 'Handle' @h@ with signature @f@, using the given implementation @impl@.
--
-- @
-- 'with' impl \\h -> ...
-- @
with ::
  (IsHandle f) =>
  -- | Implementation with effect @e@
  f e ->
  (forall e0. Handle f e0 -> Eff (e0 :& e) a) ->
  Eff e a
with handle action = mergeEff (action (mapHandle handle))

-- Internally, we just instantiate e0 with e.
