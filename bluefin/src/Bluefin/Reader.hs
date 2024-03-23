module Bluefin.Reader
  ( -- | 'Reader' is Bluefin's version of the
    -- "Control.Monad.Trans.Reader" monad.  Passing around a @Reader r
    -- e@ is equivalent to just passing around an @r@, and as such it
    -- is essentially redundant and we don't know of any reasons to
    -- use it in practice.  It is included for completeness, however.

    -- * Handle
    Reader,

    -- * Handlers
    runReader,

    -- * Effectful operations
    ask,
  )
where

import Bluefin.Internal
