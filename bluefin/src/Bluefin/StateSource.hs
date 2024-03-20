module Bluefin.StateSource
  ( -- | A 'StateSource' allows you to allocate new
    -- 'Bluefin.State.State' handles, much like 'Control.Monad.ST'
    -- allows you to allocate new 'Data.STRef.STRef's.  This can be
    -- useful when you want to avoid nested 'Bluefin.State.runState'
    -- (or `Bluefin.State.evalState' blocks), or you need an only
    -- dynamically known number of mutable states.

    -- * Handle
    StateSource,

    -- * Handlers
    withStateSource,

    -- * Effectful operations
    newState,
  )
where

import Bluefin.Internal
