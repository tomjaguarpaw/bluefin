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
