-- | This is an old interface and will be deprecated in the
-- future. You are encouraged to use "Bluefin.Capability.Ask" instead.
module Bluefin.Reader
  ( -- | 'Reader' is Bluefin's version of the
    -- "Control.Monad.Trans.Reader" monad.  'local' allows you to
    -- locally override the value in the @Reader@ capability in a
    -- well-scoped way.  The original value will be restored when you
    -- exit the @local@ block regardless of whether the exit was
    -- normal or via an exception .

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
