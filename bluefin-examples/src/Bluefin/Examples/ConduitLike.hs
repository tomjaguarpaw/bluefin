module Bluefin.Examples.ConduitLike where

import Bluefin.Compound
import Bluefin.Consume
import Bluefin.Eff
import Bluefin.IO
import Bluefin.Stream
import Control.Monad

infixr 9 `connect`

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

printC ::
  (e1 <: es, e2 <: es) =>
  (Show a) =>
  IOE e1 ->
  Consume a e2 ->
  stream ->
  Eff es r
printC io c _ = forever ((effIO io . print) =<< await c)

run ::  (() -> () -> Eff es r) ->  Eff es r
run k = k () ()

-- > example
-- (5,'A')
-- (6,'B')
-- (7,'C')
example :: IO ()
example = runEff_ $ \io -> run (\c s -> foo (mapHandle io) c s)
