module Bluefin.EarlyReturn
  ( -- | Early return allows to define a block from which you can
    -- return early.

    -- * Handle
    EarlyReturn,
    -- * Handlers
    withEarlyReturn,
    -- * Effectful operations
    returnEarly,
  )
where

import Bluefin.Internal
