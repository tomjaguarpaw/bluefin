-- | = Algebraic effects and named handlers
--
-- Algebraic effect handlers are a powerful framework for
-- user-defined effects with a simple equational intuition.
--
-- Algebraic effect handlers are expressive enough to define various effects
-- from scratch. Otherwise, implementing 'runState' for example requires
-- mutable references (@IORef@), relying on @IO@'s built-in statefulness.
-- In terms of pure expressiveness, delimited continuations are all you need.
--
-- An "algebraic effect" is a signature for a set of operations which we
-- represent with a GADT. For example, the "state effect" @State s@ contains
-- two operations: @Get@ takes no parameter and returns a value of type @s@,
-- and @Put@ takes a value of type @s@ and returns @()@. The constructors
-- @Get@ and @Put@ are "uninterpreted operations": they only describe the types
-- of arguments and results, with no intrinsic meaning.
--
-- @
-- data State s r where
--   Get :: State s s
--   Put :: s -> State s ()
-- @
--
-- Below is an example of a stateful computation: a term of some type @'Eff' zz a@ with
-- a state handler @h :: 'Handler' (State s) z@ in scope (@z :> zz@).
-- The @State@ operations can be called using 'call' and the state handler @h@.
--
-- @
-- -- Increment a counter and return its previous value.
-- incr :: z :> zz => Handler (State Int) z -> Eff zz Int
-- incr h = do
--     n <- get
--     put (n + 1)
--     pure n
--   where
--     get = call h Get
--     put s = call h (Put s)
-- @
--
-- We handle the state effect by giving an interpretation of the @Get@ and @Put@
-- operations, as a function @f :: 'HandlerBody' (State s) zz a@.
--
-- To 'call' @Get@ or @Put@ is to call the function @f@ supplied by 'handle'.
-- The following equations show how 'handle' propagates an interpretation
-- @f@ throughout a computation that calls @Get@ and @Put@:
--
-- @
-- handle f (\\h -> call h Get     >>= k h) = f Get     (handle f (\\h -> k h))
-- handle f (\\h -> call h (Put s) >>= k h) = f (Put s) (handle f (\\h -> k h))
-- handle f (\\h -> pure r) = pure r
-- @
--
-- With those equations, @'handle' f@ applied to the above @incr@ simplifies to:
--
-- @
-- 'handle' f incr =
--   f Get \\n ->
--   f (Put (n+1)) \\() ->
--   pure n
-- @
--
-- === References
--
-- - <https://www.microsoft.com/en-us/research/uploads/prod/2021/05/namedh-tr.pdf First-class names for effect handlers> (2021) by Ningning Xie, Youyou Cong, and Daan Leijen.
module Bluefin.Algae
  ( AEffect
  , HandlerBody
  , Handler
  , handle
  , call
  ) where

import Bluefin.Internal.Algae
