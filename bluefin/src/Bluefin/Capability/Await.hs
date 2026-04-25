module Bluefin.Capability.Await
  ( -- | 'Await' allows you to await values during the execution of
    -- a Bluefin operation.  It provides similar functionality to
    -- @await@ from Conduit or Pipes.
    --
    -- For information about prompt finalization/resource safety when
    -- using Bluefin @Consume@s, see "Bluefin.Coroutine".

    -- * Capability
    Await,

    -- * Handlers
    eachAwait,
    awaitYield,

    -- * Effectful operations
    await,
    takeAwait,
  )
where

import Bluefin.Internal
