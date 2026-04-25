-- | This is an old interface and will be deprecated in the
-- future. You are encouraged to use "Bluefin.Capability.Modify"
-- instead.
module Bluefin.State
  ( -- * Handle
    State,

    -- * Handlers
    evalState,
    runState,
    withState,

    -- * Effectful operations
    get,
    put,
    modify,
  )
where

import Bluefin.Internal
