module Bluefin.Stream
  ( -- * Handle
    Stream,
    -- * Handlers
    forEach,
    yieldToList,
    yieldToReverseList,
    enumerate,
    -- * Effectful operations
    yield,
    inFoldable,
  )
where

import Bluefin.Internal
