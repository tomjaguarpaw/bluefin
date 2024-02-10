module Bluefin.EarlyReturn
  ( -- | Early return allows to define a block from which you can
    -- return early.  It is not documented yet.

    -- * Handle
    EarlyReturn,
    -- * Handlers
    withEarlyReturn,
    -- * Effectful operations
    earlyReturn,
  )
where

import Bluefin.Internal
