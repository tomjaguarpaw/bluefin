module Bluefin.Capability.ReturnEarly
  ( -- | @Bluefin.ReturnEarly@ allows to define a block from which you can
    -- return early.  Early return is implemented as an exception, and
    -- its API is just an alternate interface to exceptions.

    -- * Capability
    ReturnEarly,

    -- * Handlers
    withReturnEarly,

    -- * Effectful operations
    returnEarly,
  )
where

import Bluefin.Internal
