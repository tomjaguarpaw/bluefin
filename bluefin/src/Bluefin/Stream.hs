module Bluefin.Stream
  ( -- * Handle
    Stream,
    -- * Handlers
    forEach,
    yieldToList,
    yieldToReverseList,
    enumerate,
    mapMaybe,
    catMaybes,
    -- * Effectful operations
    yield,
    inFoldable,
  )
where

import Bluefin.Internal
