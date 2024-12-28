module Bluefin.ConsumeTerminate
  ( -- | 'ConsumeTerminate' is like 'Bluefin.Consume.Consume' but it
    -- also allows you to detect upstream termination.
    -- 'awaitOrTerminate' is like @await@ from Conduit (Conduit's
    -- @await@ returns @Nothing@ when upstream has terminated,
    -- @awaitOrTerminate@ throws an exception) but not so much like
    -- @await@ from Pipes (because it doesn't give any indication of
    -- upstream termination).

    -- * Handle
    ConsumeTerminate,
    -- * Handlers
    consumeEachOrTerminate,
    consumeStreamOrTerminate,
    -- * Effectful operations
    awaitOrTerminate,
  )
where

import Bluefin.Internal
