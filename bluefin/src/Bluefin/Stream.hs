module Bluefin.Stream
  ( -- | 'Stream' allows you to yield values during the execution of a
    -- Bluefin operation.  It provides similar functionality to
    -- Python's @yield@.  The handler of the 'Stream' will either
    -- handle each element as soon as it is yielded (for example
    -- 'forEach') or gather all yielded elements int o a list (for
    -- example 'yieldToList').

    -- * Handle
    Stream,
    -- * Handlers
    forEach,
    yieldToList,
    yieldToReverseList,
    withYieldToList,
    enumerate,
    enumerateFrom,
    mapMaybe,
    catMaybes,
    -- * Effectful operations
    yield,
    inFoldable,
  )
where

import Bluefin.Internal
