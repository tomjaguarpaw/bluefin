module Bluefin.Coroutine
  ( -- | @Coroutine@ allows to yield values and receive results back.
    -- It is not documented yet.  You might want to start with
    -- "Bluefin.Stream", which is the most common way to use
    -- coroutines.

    -- * Handle
    Coroutine,

    -- * Handlers
    forEach,

    -- * Effectful operations
    yieldCoroutine,
  )
where

import Bluefin.Internal
