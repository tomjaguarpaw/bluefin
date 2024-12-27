module Bluefin.ConsumeTerminate
  ( -- * Handle
    ConsumeTerminate,
    consumeTerminate,
    -- * Handlers
    consumeEachOrTerminate,
    consumeStreamOrTerminate,
    -- * Effectful operations
    awaitOrTerminate,
  )
where

import Bluefin.Internal
