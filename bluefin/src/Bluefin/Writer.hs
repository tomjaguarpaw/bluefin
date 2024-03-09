module Bluefin.Writer
  ( -- | In most cases you'll probably prefer t'Bluefin.Stream.Stream'
    -- to @Writer@, but @Writer@ can still be useful in some cases,
    -- for example with @Data.Monoid.'Data.Monoid.Any'@ to determine
    -- whether an event ever occurred.

    -- * Handle
    Writer,
    -- * Handlers
    runWriter,
    execWriter,
    -- * Effectful operations
    tell,
  )
where

import Bluefin.Internal
