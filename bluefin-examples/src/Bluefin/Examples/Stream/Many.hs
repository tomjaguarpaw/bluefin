{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Bluefin.Examples.Stream.Many where

import Bluefin.Compound
  ( Handle (mapHandle),
    makeOp,
    useImplIn,
    useImplUnder,
    useImplWithin,
  )
import Bluefin.ConsumeTerminate
  ( ConsumeTerminate,
    awaitOrTerminate,
    consumeStreamOrTerminate,
  )
import Bluefin.Eff (Eff, bracket, runEff_, (:&), (:>))
import Bluefin.Exception (try)
import Bluefin.IO (IOE, effIO)
import Bluefin.Jump (jumpTo, withJump)
import Bluefin.State (evalState, get, modify)
import Bluefin.Stream (Stream, forEach, yield)
import Bluefin.System.IO (hGetLine, hIsEOF, withFile)
import Control.Monad (forever, replicateM_, when)
import Data.Function (fix)
import Data.Maybe (Maybe (..), isNothing)
import Data.Traversable (for)
import System.IO (IOMode (ReadMode))
import Prelude hiding
  ( break,
    drop,
    head,
    read,
    readFile,
    return,
    take,
    writeFile,
  )
import qualified Prelude

-- An example form elaforge at
--
-- https://discourse.haskell.org/t/solving-a-resourcet-related-space-leak-in-production/11007/11?u=tomjaguarpaw

-- ghci> mixExample
-- [Just "a0",Just "",Just ""]
-- [Just "a1",Just "",Just ""]
-- [Just "a2",Just "b0",Just ""]
-- File closed: a
-- [Nothing,Just "b1",Just ""]
-- [Just "b2",Just "a0"]
-- File closed: b
-- [Nothing,Just "a1"]
-- [Just "a2"]
-- File closed: a
-- [Nothing]
mixExample :: IO ()
mixExample = runEff_ $ \io -> do
  effIO io $ do
    Prelude.writeFile "a" (unlines (map (\i -> "a" <> show i) [0 :: Int .. 5]))
    Prelude.writeFile "b" (unlines (map (\i -> "b" <> show i) [0 :: Int .. 5]))

  let timings =
        [ (0, "a"),
          (2, "b"),
          (4, "a")
        ]

  -- With newtype wrapping removed, `Stream a e` is just `a -> IO ()`
  -- and `forEach` is just function application. `effIO io` is a no-op, so
  -- this is:
  --
  --     mix timings io print
  forEach (mix timings io) $ \out ->
    effIO io (print out)

-- To avoid having to use ImpredicativeTypes, which doesn't really
-- work on GHC <= 9.0
newtype Wrap h r es = MkWrap (forall e. h e -> Eff (e :& es) r)

mix ::
  forall e1 e2 es.
  (e1 :> es, e2 :> es) =>
  [(Int, FilePath)] ->
  IOE e2 ->
  Stream [Data.Maybe.Maybe String] e1 ->
  Eff es ()
mix timings io y = do
  let itersStreams :: [Wrap (Stream String) () es]
      itersStreams = map (\x -> MkWrap (pad io x)) timings

  connectMany itersStreams $ \itersStart -> do
    flip fix itersStart $ \again iters -> do
      when (not (null iters)) $ do
        outs <- for iters $ \y' -> do
          try (awaitOrTerminate y') >>= \case
            Left () -> pure Nothing
            Right r -> pure (Just r)
        yield y outs
        let iters' =
              [ iter
                | (iter, o) <- zip iters outs,
                  not (Data.Maybe.isNothing o)
              ]
        again iters'

pad ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  (Int, FilePath) ->
  Stream String e2 ->
  Eff es ()
pad io (start, fname) y = do
  replicateM_ start (yield y "")
  take 3 (linesOfFile fname io) y

-- General purpose Bluefin function for streaming the
-- lines of a file
linesOfFile ::
  (e1 :> es, e2 :> es) =>
  String ->
  IOE e1 ->
  Stream String e2 ->
  Eff es ()
linesOfFile filename io y = do
  withJump $ \onEOF -> do
    withFile io filename ReadMode $ \h -> do
      -- This bracket is only so we can observe the
      -- prompt closing of the file.
      bracket
        (pure ())
        (\() -> effIO io (putStrLn ("File closed: " <> filename)))
        ( \() -> do
            forever $ do
              isEOF <- hIsEOF h
              when isEOF $
                jumpTo onEOF
              -- A `Stream a e` is just a newtype wrapped `a -> IO ()`, and
              -- without the wrapping `yield y a` is just `y a`. So, without the
              -- wrapping this is `y =<< hGetLine h`.
              yield y =<< hGetLine h
        )

-- This should be part of the Bluefin standard library
--
-- `take n s y` takes the first n elements of `s` (yielding them to `y`)
-- and then stops. It does so by jumping out of an infinite loop
-- when a countdown (which starts at n) hits 0.
take ::
  (e1 :> es) =>
  Integer ->
  (forall e. Stream a e -> Eff (e :& es) ()) ->
  Stream a e1 ->
  Eff es ()
take n k y =
  withJump $ \done -> do
    evalState n $ \s -> do
      -- `Stream a e` is a newtype wrapped `a -> IO ()`, and
      -- with all the wrapping removed, `forEach k body` is
      -- just `k body`.
      --
      -- `useImplUnder` is a no-op, when the newtype wrapping
      -- is removed. It just massages effect type tags.
      forEach (useImplUnder . k) $ \a -> do
        s' <- get s
        when (s' <= 0) $
          jumpTo done

        modify s (subtract 1)
        -- With newtype wrapping removed this is just `y a`.
        yield y a

-- `connectMany` is the part of this program that behaves in the
-- least familiar manner. This is what it does:
--
-- When given a list of n effectful operations which yield `a`s it
-- forks n threads for them to run in. There is also a thread forked
-- for the function that accepts a list of `Consume`s. Each `Consume`
-- receives the `a`s yielded by one of the n threads.
--
-- The threads are synchronised by `MVar`s, so there is only one
-- possible interleaving of the concurrent code. When `await` is
-- called on one of the n `Consume`s the `Stream` thread corresponding
-- to that `Consume` is asked "please do the work required to get
-- me your next `a`". That unblocks it, it does its work, gives its `a`,
-- blocks, and the `Consume` thread continues.
--
-- (Actually, there are n threads forked for the `Consume` action,
-- but it only runs in the last one. This is not an essential feature,
-- it's just how the current implementation happens to work.)
connectMany ::
  -- | n effectful operations that yield `a`s
  [Wrap (Stream a) r1 es] ->
  -- | Will be called with a list of n Consumes,
  -- to which the streams above will yield their
  -- `a`s
  (forall e. [ConsumeTerminate a r1 e] -> Eff (e :& es) r2) ->
  Eff es r2
connectMany ss k =
  makeOp (connectMany' ss k [])

connectMany' ::
  [Wrap (Stream a) r1 es] ->
  (forall e. [ConsumeTerminate a r1 e] -> Eff (e :& es) r2) ->
  (forall e. [ConsumeTerminate a r1 e] -> Eff (e :& es) r2)
connectMany' [] k = k
connectMany' (MkWrap s : ss) k =
  connectMany'
    ss
    ( \cs ->
        consumeStreamOrTerminate
          (\c -> useImplIn k (mapHandle c : map mapHandle cs))
          (useImplWithin s)
    )
