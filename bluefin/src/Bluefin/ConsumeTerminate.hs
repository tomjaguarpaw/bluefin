module Bluefin.ConsumeTerminate
  ( -- * Handle
    ConsumeTerminate,
    -- * Handlers
    consumeEachOrTerminate,
    consumeStreamOrTerminate,
    -- * Effectful operations
    awaitOrTerminate,
  )
where

import Bluefin.Internal
