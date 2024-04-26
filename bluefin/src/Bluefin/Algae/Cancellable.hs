-- | = Algebraic effects and named handlers with cancellable continuations
--
-- See "Bluefin.Algae" for a general exposition on effect handlers.
-- "Bluefin.Algae" and "Bluefin.Algae.Cancellable" use the same names
-- so they should not be imported unqualified at the same time.
--
-- Cancellable continuations are useful to work with native exception handlers
-- such as 'Control.Exception.bracket' and other resource-management schemes à
-- la @resourcet@.
--
-- Cancellable continuations should be called exactly once (via 'continue' or 'discontinue'):
-- - at least once to ensure resources are eventually freed (no leaks);
-- - at most once to avoid use-after-free errors.
--
-- Enforcing this requirement with linear types would be a welcome contribution.
--
-- === Example
--
-- ==== Problem
--
-- Given 'Bluefin.Exception.Dynamic.bracket' and a @Fail@ effect,
-- the simple 'Bluefin.Algae.handle' from "Bluefin.Algae" may cause resource leaks:
--
-- @
-- 'Bluefin.Algae.handle' (\\_e _k -> pure Nothing)
--   ('Bluefin.Exception.Dynamic.bracket' ex acquire release (\\_ -> 'call' h Fail))
-- @
--
-- @bracket@ is intended to ensure that the acquired resource is released even if the bracketed
-- function throws an exception. However, when the @Fail@ operation is called, the handler
-- @(\\_e _k -> pure Nothing)@ discards the continuation @_k@ which contains the
-- exception handler installed by @bracket@.
-- The resource leaks because @release@ will never be called.
--
-- ==== Solution
--
-- Using 'handle' from this module instead lets us cancel the continuation with 'discontinue'.
-- Cancellable continuations require a 'DynExn' or 'IOE' handle.
--
-- @
-- 'handle'' io (\\_e k ->
--      try @CancelContinuation ('discontinueIO' k CancelContinuation) >> pure Nothing)
--   ('Bluefin.Exception.Dynamic.bracket' acquire release (\\_ -> 'call' h Fail))
--
-- data CancelContinuation = CancelContinuation deriving (Show, Exception)
-- @
module Bluefin.Algae.Cancellable
  ( AEffect
  , HandlerBody
  , Handler
  , handle
  , call
  , continue
  , discontinue
  , discontinueIO
  ) where

import Bluefin.Internal.Algae.Cancellable
