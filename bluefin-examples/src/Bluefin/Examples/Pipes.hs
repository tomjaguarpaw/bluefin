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
    stdinLn,
    (~>),
    (<~),

    -- ** Consumers
    Consumer,
    await,
    stdoutLn,
    print,
    (>~),
    (~<),

    -- ** Pipes
    Pipe,
    cat,
    takeWhile',
    (>->),
    (<-<),

    -- * Utilities
    next,
    each,
  )
where

-- Eventually we'll copy the actual implementation here
import Bluefin.Pipes
import Bluefin.Eff
import Bluefin.IO
import Control.Monad
import Bluefin.Capability.ReturnEarly
import Prelude hiding (break, print, takeWhile)
import Prelude qualified

stdinLn ::
  (e1 <: es, e2 <: es) =>
  IOE e1 ->
  Producer String e2 ->
  -- | ͘
  Eff es r
stdinLn io c = forever $ do
  line <- effIO io getLine
  yield c line

stdoutLn ::
  (e1 <: es, e2 <: es) =>
  IOE e1 ->
  Consumer String e2 ->
  -- | ͘
  Eff es r
stdoutLn io c = forever $ do
  line <- await c
  effIO io (putStrLn line)

takeWhile' ::
  (e <: es) =>
  (r -> Bool) ->
  Pipe r r e ->
  -- | ͘
  Eff es r
takeWhile' predicate p = withReturnEarly $ \early -> forever $ do
  a <- await p
  if predicate a
    then yield p a
    else returnEarly early a

print ::
  (e2 <: es, e1 <: es, Show a) =>
  IOE e1 ->
  Consumer a e2 ->
  -- | ͘
  Eff es r
print io p = forever $ do
  a <- await p
  effIO io (Prelude.print a)
