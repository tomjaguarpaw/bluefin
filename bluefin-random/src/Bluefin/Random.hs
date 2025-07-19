module Bluefin.Random
  ( Random,
    evalRandom,
    runRandom,
    withInitStdGen,
  )
where

import Bluefin.Eff (Eff, Effects, (:&), (:>))
import Bluefin.State (State, get, put, runState)
import qualified System.Random as Rnd
import qualified System.Random.Stateful as Rnd
import Prelude (flip, fst, pure, ($), (.), (<$>))
import Bluefin.IO (IOE, effIO)

newtype Random g e = Random (State g e)

newtype RandomPure g (e :: Effects) = RandomPure g
  deriving newtype (Rnd.RandomGen)

instance (e :> es, Rnd.RandomGen g) => Rnd.StatefulGen (Random g e) (Eff es) where
  uniformWord64 = flip Rnd.modifyGen Rnd.genWord64

  uniformByteArrayM pinned size =
    flip Rnd.modifyGen $
      Rnd.uniformByteArray pinned size

instance (e :> es, Rnd.RandomGen g) => Rnd.FrozenGen (RandomPure g e) (Eff es) where
  type MutableGen (RandomPure g e) (Eff es) = Random g e

  modifyGen (Random s) f = do
    (a, RandomPure g) <- f . RandomPure <$> get s
    put s g
    pure a

runRandom :: g -> (forall e. Random g e -> Eff (e :& es) a) -> Eff es (a, g)
runRandom g f = runState g (f . Random)

evalRandom :: g -> (forall e. Random g e -> Eff (e :& es) a) -> Eff es a
evalRandom g f = fst <$> runRandom g f

withInitStdGen ::
  (e1 :> es) =>
  IOE e1 ->
  (forall e. Random Rnd.StdGen e -> Eff (e :& es) a) ->
  Eff es a
withInitStdGen io k = do
  g <- effIO io Rnd.initStdGen
  evalRandom g k
