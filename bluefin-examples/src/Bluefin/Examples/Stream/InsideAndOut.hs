module Bluefin.Examples.Stream.InsideAndOut where

import Bluefin.Compound (useImplWithin)
import Bluefin.Consume (Consume, await, consumeEach, consumeStream)
import Bluefin.ConsumeTerminate
  ( ConsumeTerminate,
    awaitOrTerminate,
    consumeStreamOrTerminate,
  )
import Bluefin.EarlyReturn (returnEarly, withEarlyReturn)
import Bluefin.Eff (Eff, runEff, (:&), (:>))
import Bluefin.IO (IOE, effIO)
import Bluefin.Jump (jumpTo, withJump)
import Bluefin.State (evalState, get)
import Bluefin.Stream (Stream, forEach, inFoldable, yield)
import Control.Monad (forever, when)
import Data.Foldable (for_)

-- Part 1
--
-- https://journal.stuffwithstuff.com/2013/01/13/iteration-inside-and-out/

print10 :: IO ()
print10 = runEff $ \io -> do
  for_ [1 :: Int .. 10] $ \i -> do
    effIO io (print i)

printConsumeTerminate ::
  (e1 :> es, e2 :> es, Show a) =>
  IOE e2 ->
  ConsumeTerminate a () e1 ->
  Eff es ()
printConsumeTerminate io iterator =
  withJump $ \done -> do
    forever $ do
      i <- awaitOrTerminate iterator done
      effIO io (print i)

runPrintConsumeTerminate :: IO ()
runPrintConsumeTerminate = runEff $ \io -> do
  let elements :: [Int]
      elements = [1, 2, 3, 4, 5]

  consumeStreamOrTerminate
    (printConsumeTerminate io)
    (inFoldable elements)

find ::
  (Eq a) =>
  (forall e. Stream a e -> Eff (e :& es) ()) ->
  a ->
  Eff es Bool
find haystack needle = do
  withEarlyReturn $ \ret -> do
    forEach (useImplWithin haystack) $ \item -> do
      when (item == needle) $
        returnEarly ret True

    pure False

interleaveConsume ::
  (e1 :> es, e2 :> es, e3 :> es) =>
  Consume a e1 ->
  Consume a e2 ->
  Stream a e3 ->
  Eff es z
interleaveConsume c1 c2 y = do
  forever $ do
    a <- await c1
    yield y a
    interleaveConsume c2 c1 y

interleave ::
  (e1 :> es) =>
  (forall e. Stream a e -> Eff (e :& es) ()) ->
  (forall e. Stream a e -> Eff (e :& es) ()) ->
  Stream a e1 ->
  Eff es ()
interleave s1 s2 y = do
  consumeStream
    ( \c1 ->
        consumeStream
          ( \c2 ->
              interleaveConsume c1 c2 y
          )
          (useImplWithin s2)
    )
    (useImplWithin s1)

data Tree = MkTree
  { treeLeft :: Maybe Tree,
    treeLabel :: String,
    treeRight :: Maybe Tree
  }

inOrder :: (e1 :> es) => Tree -> Stream Tree e1 -> Eff es ()
inOrder tree callback = do
  case treeLeft tree of
    Nothing -> pure ()
    Just t -> inOrder t callback

  yield callback tree

  case treeRight tree of
    Nothing -> pure ()
    Just t -> inOrder t callback

printTree :: Tree -> IO ()
printTree tree = runEff $ \io -> do
  forEach (inOrder tree) $ \t -> do
    effIO io (putStrLn (treeLabel t))

contains :: (Eq a, Foldable t) => t a -> a -> Eff es Bool
contains haystack needle = do
  withEarlyReturn $ \ret -> do
    forEach (inFoldable haystack) $ \item -> do
      when (item == needle) $
        returnEarly ret True

    pure False

-- Part 2
--
-- https://journal.stuffwithstuff.com/2013/02/24/iteration-inside-and-out-part-2/

concatConsumes ::
  (e1 :> es, e2 :> es) =>
  Consume (Maybe b) e1 ->
  Consume (Maybe b) e2 ->
  (forall e. Consume (Maybe b) e -> Eff (e :& es) r) ->
  Eff es r
concatConsumes c1 c2 o = do
  evalState True $ \onFirst -> do
    -- "consumeEach" basically says "this is how we implement
    -- MoveNext".
    consumeEach (useImplWithin o) $ do
      get onFirst >>= \case
        True -> do
          -- On first
          await c1 >>= \case
            Just next -> pure (Just next)
            Nothing -> do
              await c2
        False -> do
          -- On second
          await c2

-- But it's probably better just to yield to a stream
concatConsumesToStream ::
  (e1 :> es, e2 :> es, e3 :> es) =>
  Consume (Maybe b) e1 ->
  Consume (Maybe b) e2 ->
  Stream b e3 ->
  Eff es ()
concatConsumesToStream c1 c2 y = do
  withJump $ \done -> forever $ do
    await c1 >>= \case
      Just next -> yield y next
      Nothing -> jumpTo done

  withJump $ \done -> forever $ do
    await c2 >>= \case
      Just next -> yield y next
      Nothing -> jumpTo done

concatStreams ::
  (e1 :> es) =>
  (forall e. Stream a e -> Eff (e :& es) ()) ->
  (forall e. Stream a e -> Eff (e :& es) ()) ->
  Stream a e1 ->
  Eff es ()
concatStreams s1 s2 y = do
  forEach s1 $ \item -> yield y item
  forEach s2 $ \item -> yield y item

concatDeep ::
  (e1 :> es) =>
  (forall e. Stream a e -> Eff (e :& es) ()) ->
  (forall e. Stream a e -> Eff (e :& es) ()) ->
  Stream a e1 ->
  Eff es ()
concatDeep a b y = do
  -- No need for `yield from`.  Just call the function!
  walkFirst a y
  walkSecond b y
  where
    walkFirst ::
      (e1 :> es) =>
      (forall e. Stream a e -> Eff (e :& es) r) ->
      Stream a e1 ->
      Eff es r
    walkFirst a' y' = forEach a' $ \item -> yield y' item

    walkSecond ::
      (e1 :> es) =>
      (forall e. Stream a e -> Eff (e :& es) r) ->
      Stream a e1 ->
      Eff es r
    walkSecond b' y' = forEach b' $ \item -> yield y' item
