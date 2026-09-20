module Bluefin.StateSource
  ( -- | A 'StateSource' allows you to allocate new
    -- t'Bluefin.Capability.Modify.Modify' handles, much like t'Control.Monad.ST'
    -- allows you to allocate new 'Data.STRef.STRef's.  This can be
    -- useful when you want to avoid nested 'Bluefin.Capability.Modify.runModify'
    -- (or `Bluefin.Capability.Modify.evalModify') blocks, or you need a number
    -- of mutable states that is only dynamically known.

    -- * Handle
    StateSource,

    -- * Handlers
    withStateSource,

    -- * Effectful operations
    newState,
  )
where

import Bluefin.Internal
