{-# LANGUAGE
  BangPatterns,
  GADTs,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | = Algebraic effects and named handlers
module Bluefin.Internal.Algae
  ( AEffect
  , HandlerBody
  , Handler
  , handle
  , call
  ) where

import Data.Kind (Type)
import Bluefin.Internal (Eff, Effects, type (:&), type (:>))
import Bluefin.Internal.DelCont

-- | Algebraic effect.
type AEffect = Type -> Type

-- | Interpretation of an algebraic effect @f@: a function to handle the operations of @f@.
type HandlerBody :: AEffect -> Effects -> Type -> Type
type HandlerBody f ss a = (forall x. f x -> (x -> Eff ss a) -> Eff ss a)

-- | Handler to call operations of the effect @f@.
type Handler :: AEffect -> Effects -> Type
data Handler f s where
  Handler :: !(PromptTag ss a s) -> HandlerBody f ss a -> Handler f s

-- | Handle operations of @f@.
--
-- === Warning for exception-like effects
--
-- If the handler might not call the continuation (like for an exception effect), and
-- if the handled computation manages resources (e.g., opening files, transactions)
-- prefer 'handle'' to trigger resource clean up with cancellable continuations.
handle ::
  HandlerBody f ss a ->
  (forall s. Handler f s -> Eff (s :& ss) a) ->
  Eff ss a
handle h act = reset (\p -> act (Handler p h))

-- | Call an operation of @f@.
call :: s :> ss => Handler f s -> f a -> Eff ss a
call (Handler p h) op = shift0 p (\k -> h op (k . pure))
