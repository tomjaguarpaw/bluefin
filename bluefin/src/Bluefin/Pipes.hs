-- | Reimplementation of the @pipes@ (@Pipes@) ecosystem in Bluefin.
-- See also "Bluefin.Pipes.Prelude".
module Bluefin.Pipes
  ( -- * The Proxy handle
    Proxy,
    Effect,
    runEffect,

    -- ** Producers
    Producer,
    yield,
    for,
    (~>),
    (<~),

    -- ** Consumers
    Consumer,
    await,
    (>~),
    (~<),

    -- ** Pipes
    Pipe,
    cat,
    (>->),
    (<-<),

    -- * Utilities
    next,
    each,
  )
where

import Bluefin.Internal.Pipes
