-- | Reimplementation of the @pipes@ (@Pipes@) ecosystem in Bluefin.
--
-- You should not use this module.  It will be deprecated and removed
-- in future versions.
--
-- This module is just an example of what you can do with Bluefin and
-- as such it should be obtained from
-- [@bluefin-examples@](https://github.com/tomjaguarpaw/bluefin/tree/master/bluefin-examples)
-- if you want it.  Instead of using it directly you are recommended
-- to use
--
-- * t'Bluefin.Capability.Yield.Yield', 'Bluefin.Capability.Yield.yield'
-- * t'Bluefin.Capability.Await.Await', 'Bluefin.Capability.Await.await'
-- * 'Bluefin.Capability.Yield.awaitYield'
-- * For advanced cases only, t'Bluefin.Capability.Request.Request',
--   'Bluefin.Capability.Request.request' and
--   'Bluefin.Capability.Request.connectRequests'
--
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
