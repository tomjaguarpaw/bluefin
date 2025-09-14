{-# LANGUAGE DerivingVia #-}

module Bluefin.Examples.CloneableHandle where

import Bluefin.CloneableHandle
  ( CloneableHandle,
    Generic1,
    GenericCloneableHandle (..),
    withEffToIOCloneHandle,
  )
import Bluefin.Compound
  ( Generic,
    Handle (mapHandle),
    OneWayCoercible (..),
    OneWayCoercibleHandle (..),
    gOneWayCoercible,
  )
import Bluefin.Eff (Eff, runEff_, (:>))
import Bluefin.Exception (Exception, throw, try)
import Bluefin.IO (IOE, effIO)
import Bluefin.State (State, evalState, get, modify)
import Control.Concurrent.Async qualified

data MyHandle e = MkMyHandle (Exception String e) (State Int e)
  deriving (Generic, Generic1)
  deriving (Handle) via OneWayCoercibleHandle MyHandle
  deriving (CloneableHandle) via GenericCloneableHandle MyHandle

instance (e :> es) => OneWayCoercible (MyHandle e) (MyHandle es) where
  oneWayCoercibleImpl = gOneWayCoercible

-- -- Run one time
-- ghci> example
-- Right 2
-- State started at 0 and was cloned. Now: 0
-- -- Run another time
-- ghci> example
-- Left "Aborting from branch 1"
-- State started at 0 and was cloned. Now: 0
example :: IO ()
example = runEff_ $ \io -> evalState 0 $ \st -> do
  r <- try $ \ex -> do
    bluefinRace
      io
      (MkMyHandle (mapHandle ex) (mapHandle st))
      ( \_ (MkMyHandle ex' st') -> do
          modify st' (subtract 2000)
          throw ex' "Aborting from branch 1"
      )
      ( \_ (MkMyHandle _ st') -> do
          modify st' (+ 3000)
          pure (2 :: Int)
      )

  s <- get st
  effIO io (print r)
  effIO io (putStrLn ("State started at 0 and was cloned. Now: " <> show s))

bluefinRace ::
  (CloneableHandle h, e1 :> es) =>
  IOE e1 ->
  h es ->
  (forall e. IOE e -> h e -> Eff e r) ->
  (forall e. IOE e -> h e -> Eff e r) ->
  Eff es r
bluefinRace io h m1 m2 = withEffToIOCloneHandle io h $ \runInIO -> do
  either id id
    <$> Control.Concurrent.Async.race
      (runInIO $ \io' h' -> m1 io' h')
      (runInIO $ \io' h' -> m2 io' h')
