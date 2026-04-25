module Bluefin.Capability.Yield
  ( -- | 'Yield' allows you to yield values during the execution of a
    -- Bluefin operation.  It provides similar functionality to
    -- Python's @yield@.  The handler of the 'Yield' will either
    -- handle each element as soon as it is yielded (for example
    -- 'forEach') or gather all yielded elements into a list (for
    -- example 'yieldToList').
    --
    -- For information about prompt finalization/resource safety when
    -- using Bluefin @Yield@s, see "Bluefin.Capability.Request".

    -- * Capability
    Yield,

    -- * Handlers
    forEach,
    yieldToList,
    yieldToReverseList,
    withYieldToList,
    ignoreYield,
    enumerate,
    enumerateFrom,
    mapMaybe,
    catMaybes,
    awaitYield,

    -- * Effectful operations
    yield,
    inFoldable,
    cycleToYield,
    takeAwait,
  )
where

import Bluefin.Internal
