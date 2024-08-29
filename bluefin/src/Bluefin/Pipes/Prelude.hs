-- | Reimplementation of the @pipes@ prelude ("Pipes.Prelude") in
-- Bluefin.  See also "Bluefin.Pipes".
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
