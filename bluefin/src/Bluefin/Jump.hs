-- | This is an old interface and will be deprecated in the
-- future. You are encouraged to use "Bluefin.Capability.JumpTo" instead.
module Bluefin.Jump
  ( -- | 'Jump' allows you to jump back to a previously-set location.
    -- A "jump" is equivalent to an untyped early return, or more
    -- precisely an early return of type @()@, which is itself an
    -- exception of type @()@.

    -- * Handle
    Jump,

    -- * Handlers
    withJump,

    -- * Effectful operations
    jumpTo,
  )
where

import Bluefin.Internal
