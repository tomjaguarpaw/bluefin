module Bluefin.Compound
  ( -- | @Compound@ allows combining two effects into one, for
    -- encapsulation.  It is not documented yet.

    -- * Handle
    Compound,

    -- * Handler
    runCompound,

    -- * Effectful operations
    withCompound,
  )
where

import Bluefin.Internal
