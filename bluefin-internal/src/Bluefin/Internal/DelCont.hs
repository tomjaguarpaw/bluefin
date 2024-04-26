{-# LANGUAGE
  BangPatterns,
  MagicHash,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators,
  UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | = Delimited continuations
module Bluefin.Internal.DelCont
  ( PromptTag
  , reset
  , shift0
  ) where

import Data.Kind (Type)
import GHC.Exts (State#, RealWorld, PromptTag#, prompt#, control0#, newPromptTag#)
import GHC.IO (IO(IO))
import Bluefin.Internal (Eff(..), Effects, type (:&), type (:>))

-- | Tag for a prompt of type @Eff ss a@ and scope @s@.
type PromptTag :: Effects -> Type -> Effects -> Type
data PromptTag ss a s = PromptTag (PromptTag# a)

unsafeMkEff :: (State# RealWorld -> (# State# RealWorld , a #)) -> Eff ss a
unsafeMkEff f = UnsafeMkEff (IO f)

unsafeRunEff :: Eff ss a -> State# RealWorld -> (# State# RealWorld , a #)
unsafeRunEff (UnsafeMkEff (IO f)) = f

-- | Run the enclosed computation under a prompt of type @Eff ss a@.
--
-- @
-- f : forall s. 'PromptTag' ss a s -> 'Eff' (s ':&' ss) a
-- -------------------------------------------------
-- 'reset' (\\t -> f t) : 'Eff' ss a
-- @
--
-- The enclosed computation @f@ is given a tag which identifies that prompt
-- and remembers its type.
-- The scope parameter @s@ prevents the tag from being used outside of the
-- computation.
--
-- A prompt ('reset') delimits a slice of the call stack (or evaluation context),
-- which can be captured with 'shift0'. This slice, a continuation,
-- becomes a function of type @Eff ss0 b -> Eff ss a@ (where @Eff ss0 b@ is the
-- result type of 'shift0' at its calling site).
-- Calling the continuation restores the slice on the stack.
reset :: forall a ss.
  (forall s. PromptTag ss a s -> Eff (s :& ss) a) ->
  Eff ss a
reset f = unsafeMkEff (\z0 -> case newPromptTag# z0 of
    (# z1, tag #) -> prompt# tag (unsafeRunEff (f (PromptTag tag))) z1)

-- | Capture the continuation up to the tagged prompt.
--
-- @
-- _ : s :> ss0
-- t : 'PromptTag' ss a s
-- f : ('Eff' ss0 b -> 'Eff' ss a) -> 'Eff' ss a
-- ---------------------------------------
-- 'shift0' t (\\k -> f k) : 'Eff' ss0 b
-- @
--
-- The prompt ('reset') is reinserted on the stack when the continuation is called:
--
-- @
-- 'reset' (\\t -> C ('shift0' t f)) = f (\\x -> 'reset' (\\t -> C x))
-- @
shift0 :: forall s a b ss ss0.
  s :> ss0 =>
  PromptTag ss a s ->
  ((Eff ss0 b -> Eff ss a) -> Eff ss a) ->
  Eff ss0 b
shift0 (PromptTag tag) f = unsafeMkEff (\z0 ->
  control0# tag (\k# ->
    unsafeRunEff (f (unsafeMkEff . prompt# tag . k# . unsafeRunEff))) z0)
