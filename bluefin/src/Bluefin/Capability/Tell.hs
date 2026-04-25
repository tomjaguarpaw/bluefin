module Bluefin.Capability.Tell
  ( -- | In most cases you'll probably prefer t'Bluefin.Capability.Yield.Yield'
    -- to @Tell@, but @Tell@ can still be useful in some cases,
    -- for example with @Data.Monoid.'Data.Monoid.Any'@ to determine
    -- whether an event ever occurred.

    -- * Capability
    Tell,

    -- * Handlers
    runTell,
    execTell,

    -- * Effectful operations
    tell,
  )
where

import Bluefin.Internal
