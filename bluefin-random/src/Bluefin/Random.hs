{-# LANGUAGE TypeFamilies #-}

module Bluefin.Random
  ( -- * Handle
    Random,

    -- * Handlers
    withInitStdGen,

    -- ** Special purpose handlers
    -- $specialpurposehandlers
    evalRandom,
    runRandom,

    -- * Effectful operations
    -- $effectfuloperations

    -- * Internal details
    -- $internaldetails
    RandomPure,
  )
where

import Bluefin.Compound (Handle)
import Bluefin.Eff (Eff, Effects, (:&), (:>))
import Bluefin.IO (IOE, effIO)
import Bluefin.State (State, get, put, runState)
import qualified System.Random as Rnd
import qualified System.Random.Stateful as Rnd

-- $specialpurposehandlers
--
-- In the vast majority of cases you should use 'withInitStdGen' and
-- you won't have any need for these special purpose handlers.

-- $effectfuloperations
--
-- To run random operations in Bluefin you should use the random
-- operations in the "System.Random.Stateful" module from the @random@
-- package. Here are their type signatures when restricted to
-- Bluefin's @Random@:
--
-- @
-- 'System.Random.Stateful.uniformM' ::
--   (Uniform a, RandomGen g, e1 :> es) =>
--   Random g e1 ->
--   Eff es a
-- @
--
-- @
-- 'System.Random.Stateful.uniformRM' ::
--   (UniformRange a, RandomGen g, e1 :> es) =>
--   (a, a) ->
--   Random g e1 ->
--   Eff es a
-- @
--
-- @
-- 'System.Random.Stateful.uniformListM' ::
--   (Uniform a, RandomGen g, e1 :> es) =>
--   Int ->
--   Random g e1 ->
--   Eff es [a]
-- @
--
-- @
-- 'System.Random.Stateful.uniformListRM' ::
--   (UniformRange a, RandomGen g, e1 :> es) =>
--   Int ->
--   (a, a) ->
--   Random g e1 ->
--   Eff es [a]
-- @
--
-- @
-- 'System.Random.Stateful.uniformShuffleListM' ::
--   (RandomGen g, e1 :> es) =>
--   [a] ->
--   Random g e1 ->
--   Eff es [a]
-- @
--
-- @
-- 'System.Random.Stateful.uniformByteArrayM' ::
--   (RandomGen g, e1 :> es) =>
--   Bool ->
--   Int ->
--   Random g e1 ->
--   Eff es ByteArray
-- @
--
-- @
-- 'System.Random.Stateful.uniformByteStringM' ::
--   (RandomGen g, e1 :> es) =>
--   Int ->
--   Random g e1 ->
--   Eff es ByteString
-- @
--
-- @
-- 'System.Random.Stateful.uniformShortByteStringM' ::
--   (RandomGen g, e1 :> es) =>
--   Int ->
--   Random g e1 ->
--   Eff es ShortByteString
-- @
--
-- @
-- 'System.Random.Stateful.uniformDouble01M' ::
--   (RandomGen g, e1 :> es) =>
--   Random g e1 ->
--   Eff es Double
-- @
--
-- @
-- 'System.Random.Stateful.uniformDoublePositive01M' ::
--   (RandomGen g, e1 :> es) =>
--   Random g e1 ->
--   Eff es Double
-- @
--
-- @
-- 'System.Random.Stateful.uniformFloat01M' ::
--   (RandomGen g, e1 :> es) =>
--   Random g e1 ->
--   Eff es Float
-- @
--
-- @
-- 'System.Random.Stateful.uniformFloatPositive01M' ::
--   (RandomGen g, e1 :> es) =>
--   Random g e1 ->
--   Eff es Float
-- @

-- $internaldetails
--
-- 'RandomPure' is an internal detail that is used to implement a
-- 'System.Random.Stateful.FrozenGen' instance for 'Random'.  You may
-- see it in error messages, so we include it here from completeness.
-- You will most likely never need to use @RandomPure@ directly.

newtype Random g e = Random (State g e)
  deriving newtype (Handle)

newtype RandomPure g (e :: Effects) = RandomPure g
  deriving newtype (Rnd.RandomGen)

instance
  (e :> es, Rnd.RandomGen g) =>
  Rnd.StatefulGen (Random g e) (Eff es)
  where
  uniformWord64 =
    flip Rnd.modifyGen Rnd.genWord64

  uniformByteArrayM pinned size =
    flip Rnd.modifyGen (Rnd.uniformByteArray pinned size)

instance
  (e :> es, Rnd.RandomGen g) =>
  Rnd.FrozenGen (RandomPure g e) (Eff es)
  where
  type MutableGen (RandomPure g e) (Eff es) = Random g e

  modifyGen (Random s) f = do
    (a, RandomPure g) <- f . RandomPure <$> get s
    put s g
    pure a

runRandom ::
  g ->
  (forall e. Random g e -> Eff (e :& es) a) ->
  -- | ͘
  Eff es (a, g)
runRandom g f = runState g (f . Random)

evalRandom ::
  g ->
  (forall e. Random g e -> Eff (e :& es) a) ->
  -- | ͘
  Eff es a
evalRandom g f = fst <$> runRandom g f

-- | The simplest way to handle a Bluefin 'Random' effect.  This is
-- the handler you should use unless you know you have a particular
-- need to use a @Rnd.RandomGen@ other than @Rnd.StdGen@ or you know
-- you need to create a @Rnd.StdGen@ seed in a non-standard way.
withInitStdGen ::
  (e1 :> es) =>
  IOE e1 ->
  (forall e. Random Rnd.StdGen e -> Eff (e :& es) a) ->
  -- | ͘
  Eff es a
withInitStdGen io k = do
  g <- effIO io Rnd.initStdGen
  evalRandom g k
