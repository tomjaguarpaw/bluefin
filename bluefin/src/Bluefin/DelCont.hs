-- | = Delimited continuations
--
-- Native multi-prompt delimited continuations.
-- These primitives let us manipulate slices of the call stack/evaluation
-- context delimited by 'reset'.
--
-- This module serves as a foundation for algebraic effect handlers,
-- a more structured interface for manipulating continuations and implementing
-- user-defined effects.
--
-- The behavior of 'reset' and 'shift0' is summarized by the following equations:
--
-- @
-- 'reset' (\\_ -> 'pure' x) = 'pure' x
-- 'reset' (\\t -> C ('shift0' t f)) = f (\\x -> 'reset' (\\t -> C x))
-- @
--
-- where @C@ is an evaluation context (in which @t@ may occur), i.e.,
-- a term of the following form:
--
-- > C x ::= C x >>= k      -- for any function k
-- >       | H (\h -> C x)  -- for any handler H ∈ { reset, (`runState` s), ... }
-- >       | x
--
--
-- This module ensures type safety. The rank-2 type of 'reset'
-- guarantees that 'shift0' will always have a maching 'reset' on the stack.
--
-- === References
--
-- - <https://ghc-proposals.readthedocs.io/en/latest/proposals/0313-delimited-continuation-primops.html Delimited continuation primops> (GHC proposal, implemented in GHC 9.6.1).
-- - <https://homes.luddy.indiana.edu/ccshan/recur/recur.pdf Shift to Control> (2004) by Chung-chieh Shan. The name 'shift0' follows the nomenclature in that paper.

module Bluefin.DelCont
  ( PromptTag
  , reset
  , shift0
  ) where

import Bluefin.Internal.DelCont
