-- | This is an old interface and will be deprecated in the
-- future. You are encouraged to use "Bluefin.Capability.Throw"
-- instead.
module Bluefin.Exception
  ( -- * Handle
    Exception,

    -- * Handlers
    try,
    handle,
    catch,

    -- * Effectful operations
    throw,
    rethrowIO,
  )
where

import Bluefin.Internal
