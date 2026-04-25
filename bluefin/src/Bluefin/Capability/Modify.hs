module Bluefin.Capability.Modify
  ( -- * Capability
    Modify,

    -- * Handlers
    evalModify,
    runModify,
    withModify,

    -- * Effectful operations
    get,
    put,
    modify,
  )
where

import Bluefin.Internal
