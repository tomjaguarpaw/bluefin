module Bluefin.Consume
  ( -- | 'Consume' allows you to await values during the execution of
    -- a Bluefin operation.  It provides similar functionality to
    -- @await@ from Conduit or Pipes.

    -- * Handle
    Consume,
    -- * Handlers
    consumeEach,
    consumeStream,
    -- * Effectful operations
    await,
  )
where

import Bluefin.Internal
