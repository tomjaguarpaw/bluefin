-- | Reimplementation of the @pipes@ (@Pipes@) ecosystem in Bluefin.
-- It primarily serves as an example of what you can do with Bluefin
-- but you probably won't want to use an API like this directly.
-- Instead you are recommended to use
--
-- * 'Bluefin.Stream', 'Bluefin.Stream.yield'
-- * 'Bluefin.Consume', 'Bluefin.Consume.await'
-- * 'Bluefin.Stream.consumeStream'
-- * For advanced cases only, 'Bluefin.Coroutine',
--   'Bluefin.Coroutine.yieldCoroutine' and
--   'Bluefin.Coroutine.connectCoroutines'
--
-- See also "Bluefin.Pipes.Prelude".
module Bluefin.Examples.Pipes
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

-- Eventually we'll copy the actual implementation here
import Bluefin.Pipes
