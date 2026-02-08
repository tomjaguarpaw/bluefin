module Bluefin.Consume
  ( -- | 'Consume' allows you to await values during the execution of
    -- a Bluefin operation.  It provides similar functionality to
    -- @await@ from Conduit or Pipes.
    --
    -- For information about prompt finalization/resource safety when
    -- using Bluefin @Consume@s, see "Bluefin.Coroutine".

    -- * Handle
    Consume,

    -- * Handlers
    consumeEach,
    consumeStream,
    streamConsume,

    -- * Effectful operations
    await,
    takeConsume,
  )
where

import Bluefin.Internal
