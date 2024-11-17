module Bluefin.Reader
  ( -- | 'Reader' is Bluefin's version of the
    -- "Control.Monad.Trans.Reader" monad.

    -- * Handle
    Reader,

    -- * Handlers
    runReader,

    -- * Effectful operations
    ask,
    asks,
    local,
  )
where

import Bluefin.Internal
