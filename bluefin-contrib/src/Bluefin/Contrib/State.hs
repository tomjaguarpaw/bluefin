module Bluefin.Contrib.State where

import Bluefin.Eff (Eff, Effects, (:&))
import Bluefin.State (State, get)
import Bluefin.StateSource (newState, withStateSource)

-- |
-- @
-- >>> 'Bluefin.EFf.runPureEff' $ runManyStates [10,15,20] $ 'Data.Traversable.traverse' $ \\st -> do
--        n <- 'Bluefin.State.get' st
--        pure (2 * n)
-- ([20,30,40],[10,15,20])
-- @
runManyStates ::
  (Traversable t) =>
  t s ->
  (forall (e :: Effects). t (State s e) -> Eff (e :& es) a) ->
  -- | ͘
  Eff es (a, t s)
runManyStates ss f =
  withStateSource $ \source -> do
    states <- traverse (newState source) ss
    a <- f states
    ss' <- traverse get states
    pure (a, ss')
