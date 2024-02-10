module Bluefin.Compound
  ( -- | @Compound@ allows combining two effects into one, for
    -- encapsulation.  It is not documented yet.

    -- * Handle
    Compound,

    -- * Handlers
    runC0,

    -- * Effectful operations
    withC,
  )
where

import Bluefin.Internal
