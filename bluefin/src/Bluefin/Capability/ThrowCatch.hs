-- | You probably don't want to use this module, rather you'll want
-- "Bluefin.Capability.Throw" for most exception use
-- cases. @ThrowCatch@ exists mainly to support @MonadError@
-- instances.
module Bluefin.Capability.ThrowCatch
  ( -- * Capability
    ThrowCatch,

    -- * Handlers
    try,
    handle,
    catch,

    -- * Effectful operations
    throw,

    -- * Local handling
    localTry,
    localCatch,
    localHandle,
  )
where

import Bluefin.Internal.Capability.ThrowCatch
  ( ThrowCatch,
    handle,
    catch,
    localCatch,
    localHandle,
    localTry,
    throw,
    try,
  )
