{-# LANGUAGE DerivingVia #-}

module Bluefin.Examples.Server where

import Bluefin.Compound (Handle (..), handleMapHandle)
import Bluefin.Consume (Consume, await, streamConsume)
import Bluefin.Eff (Eff, runEff_, (:>))
import Bluefin.IO (effIO)
import Bluefin.Stream (Stream, forEach, yield)
import Control.Monad (forever)
import Data.Map (Map)
import Data.Map qualified as Map

broker ::
  (e1 :> es, e2 :> es, Show v) =>
  Stream String e2 ->
  Consume (String, v) e1 ->
  MapHandle String (Stream v) es ->
  Eff es ()
broker yout c = process_yout_c
  where
    process_yout_c m = do
      (k, v) <- await c

      case lookupHandle k m of
        Just s -> do
          -- We've seen this thread ID before so yield to its stream
          yield s v
          broker yout c m
        Nothing -> do
          -- We haven't seen this chat ID before so spawn a new
          -- handler
          streamConsume
            (\s' -> broker yout c (insertHandle k s' m))
            (processChatId k v yout)

-- An example program for how to handle chat messages based on the
-- chat ID
processChatId ::
  (e1 :> es, e2 :> es, Show v) =>
  String ->
  v ->
  Stream String e1 ->
  Consume v e2 ->
  Eff es r
processChatId chatId v yout c = do
  if chatId == "C"
    -- To show that we can treat different chat IDs differently, let's
    -- have a special case for C. We just ignore its messages.
    then do
      yield yout "chatId was C. Special case"
      forever $ do
        _ <- await c
        yield yout "Ignored a C message"
        pure ()
    else do
      -- For every other message we yield its first and subsequent
      -- messages
      yield yout ("First " <> chatId <> ": " <> show v)
      forever $ do
        vv <- await c
        yield yout ("Subsequent " <> chatId <> ": " <> show vv)

incoming :: (e1 :> es) => Stream (String, Int) e1 -> Eff es ()
incoming y = do
  yield y ("A", 1)
  yield y ("B", 10)
  yield y ("A", 2)
  yield y ("B", 20)
  yield y ("C", 100)
  yield y ("C", 200)
  yield y ("B", 30)
  yield y ("A", 3)
  yield y ("C", 300)

-- > Bluefin.Examples.Server.example
-- First A: 1
-- First B: 10
-- Subsequent A: 2
-- Subsequent B: 20
-- chatId was C. Special case
-- Ignored a C message
-- Subsequent B: 30
-- Subsequent A: 3
-- Ignored a C message
example :: IO ()
example = runEff_ $ \io -> do
  forEach
    ( \yout -> do
        streamConsume
          incoming
          (\c -> broker yout c emptyMapHandle)
    )
    (effIO io . putStrLn)

-- Useful for keeping track of many handles

newtype MapHandle k h e = MkMapHandle (Map k (h e))

instance (Handle h) => Handle (MapHandle k h) where
  handleImpl = handleMapHandle $ \(MkMapHandle m) ->
    MkMapHandle (fmap mapHandle m)

insertHandle ::
  (Ord k, Handle h, e1 :> es, e2 :> es) =>
  k ->
  h e1 ->
  MapHandle k h e2 ->
  MapHandle k h es
insertHandle k h (MkMapHandle m) =
  MkMapHandle (Map.insert k (mapHandle h) (fmap mapHandle m))

lookupHandle :: (Ord k) => k -> MapHandle k h e -> Maybe (h e)
lookupHandle k (MkMapHandle m) = Map.lookup k m

emptyMapHandle :: (Ord k) => MapHandle k h e
emptyMapHandle = MkMapHandle mempty
