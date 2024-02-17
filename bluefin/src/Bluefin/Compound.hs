module Bluefin.Compound
  ( -- | @Compound@ allows combining two effects into one, for
    -- encapsulation.  It is not documented yet.

    -- * Handle
    Compound,

    -- * Effectful operations
    compound,
    withCompound,
  )
where

import Bluefin.Internal
