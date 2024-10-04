-- | Reimplementation of the @pipes@ prelude (@Pipes.Prelude@) in
-- Bluefin.  It primarily serves as an example of what you can do with
-- Bluefin and you probably won't want to use it directly.  Instead
-- you are recommended to use
--
-- * 'Bluefin.Stream', 'Bluefin.Stream.yield'
-- * 'Bluefin.Consume', 'Bluefin.Consume.await'
-- * 'Bluefin.Stream.consumeStream'
-- * For advanced cases only, 'Bluefin.Coroutine',
--   'Bluefin.Coroutine.yieldCoroutine' and
--   'Bluefin.Coroutine.connectCoroutines'
--
-- See also "Bluefin.Pipes".
--
-- @
-- >>> 'Bluefin.Eff.runEff' $ \\io -> 'runEffect' $ do
--       'stdinLn' io >-> 'takeWhile'' (/= "quit") >-> 'stdoutLn' io
-- Test
-- Test
-- ABC
-- ABC
-- quit
-- "quit"
-- @
module Bluefin.Pipes.Prelude
  ( -- * Producers
    stdinLn,
    repeatM,
    replicateM,
    unfoldr,

    -- * Consumers
    stdoutLn,
    mapM_,
    print,
    drain,

    -- * Pipes
    map,
    mapM,
    takeWhile',
  )
where

import Bluefin.Internal.Pipes
