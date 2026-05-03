module Bluefin.Examples.ConduitLike where

import Bluefin.Compound (mapHandle)
import Bluefin.Consume (Consume, await)
import Bluefin.Eff (Eff, runEff_, (:&), type (<:))
import Bluefin.IO (IOE, effIO)
import Bluefin.Jump (jumpTo, withJump)
import Bluefin.Stream
  ( Stream,
    enumerateFrom,
    inFoldable,
    streamConsume,
    takeConsume,
    yield,
  )
import Bluefin.System.IO (hIsEOF, hGetLine, withFile)
import Control.Monad (forever)
import Data.Foldable (for_)
import System.Directory (listDirectory)
import System.IO qualified

infixr 9 `connect`

main :: IO ()
main = runEff_ $ \io ->
  run $
    sourceDirectory "/tmp/manyfiles" io
      `connect` firstNLines 1 io
      `connect` printC io

firstNLines ::
  (e1 <: es, e2 <: es, e3 <: es) =>
  Int ->
  IOE e1 ->
  Consume FilePath e2 ->
  Stream String e3 ->
  Eff es ()
firstNLines n io = awaitForever $ \filepath ->
  sourceFileLines filepath io
    `connect` takeConsume n

sourceDirectory ::
  (e1 <: es, e2 <: es) =>
  FilePath ->
  IOE e1 ->
  consume ->
  Stream String e2 ->
  Eff es ()
sourceDirectory filepath io _ y = do
  dirs <- effIO io (listDirectory filepath)
  for_ dirs $ \dir -> yield y (filepath <> "/" <> dir)

sourceFileLines ::
  (e1 <: es, e2 <: es) =>
  FilePath ->
  IOE e1 ->
  consume ->
  Stream String e2 ->
  Eff es ()
sourceFileLines filename io _ y = do
  withFile io filename System.IO.ReadMode $ \h -> do
    withJump $ \done -> forever $ do
      eof <- hIsEOF h
      if eof
        then jumpTo done
        else do
          line <- hGetLine h
          yield y line

awaitForever ::
  (e1 <: es) =>
  (a -> () -> stream -> Eff es b) ->
  Consume a e1 ->
  stream ->
  Eff es r
awaitForever f a y = forever $ do
  x <- await a
  f x () y

connect ::
  ( forall e.
    consume ->
    Stream b e ->
    Eff (e :& es) r
  ) ->
  ( forall e.
    Consume b e ->
    stream ->
    Eff (e :& es) r
  ) ->
  consume ->
  stream ->
  Eff es r
connect f1 f2 c s = streamConsume (\s' -> f1 c s') (\c' -> f2 c' s)

run :: (() -> () -> Eff es r) -> Eff es r
run k = k () ()

printC ::
  (e1 <: es, e2 <: es) =>
  (Show a) =>
  IOE e1 ->
  Consume a e2 ->
  stream ->
  Eff es r
printC io c _ = forever ((effIO io . print) =<< await c)

-- Other interesting bits

asConsume ::
  (e1 <: es) =>
  ((forall e. Stream a e -> Eff (e :& es) ()) -> stream -> Eff es ()) ->
  Consume a e1 ->
  stream ->
  Eff es ()
asConsume k c s = k (\y -> forever (await c >>= yield y)) s

foo :: IOE e -> consume -> stream -> Eff e ()
foo io =
  (\_ -> inFoldable ['A' .. 'Z'])
    `connect` asConsume (enumerateFrom 5)
    `connect` takeConsume 5
    `connect` takeConsume 4
    `connect` takeConsume 3
    `connect` printC io

-- > example
-- (5,'A')
-- (6,'B')
-- (7,'C')
example :: IO ()
example = runEff_ $ \io -> run (\c s -> foo (mapHandle io) c s)
