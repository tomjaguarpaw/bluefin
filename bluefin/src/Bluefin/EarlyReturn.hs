module Bluefin.EarlyReturn
  ( -- | Early return allows to define a block from which you can
    -- return early.  Early return is implemented as an exception, and
    -- its API is just an alternate interface to exceptions.

    -- * Handle
    EarlyReturn,
    -- * Handlers
    withEarlyReturn,
    -- * Effectful operations
    returnEarly,
  )
where

import Bluefin.Internal
