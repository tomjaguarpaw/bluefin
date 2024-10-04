-- | Reimplementation of the @pipes@ (@Pipes@) ecosystem in Bluefin.
-- It primarily serves as an example of what you can do with Bluefin
-- and you probably won't want to use it directly.  Instead you are
-- recommended to use
--
-- * 'Bluefin.Stream', 'Bluefin.Stream.yield'
-- * 'Bluefin.Consume', 'Bluefin.Consume.await'
-- * 'Bluefin.Stream.consumeStream'
-- * For advanced cases only, 'Bluefin.Coroutine',
--   'Bluefin.Coroutine.yieldCoroutine' and
--   'Bluefin.Coroutine.connectCoroutines'
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
