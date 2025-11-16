module Bluefin.Examples.FunctorCoroutine where

import Bluefin.Eff (Eff, runEff, (:&), (:>))
import Bluefin.FunctorCoroutine
import Bluefin.HandleReader (HandleReader, askHandle, runHandleReader)
import Bluefin.IO (IOE, effIO)
import Data.Kind (Type)

data FileSystem :: Effect where
  ReadFile :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

readFile ::
  (e1 :> es) => Send FileSystem e1 -> FilePath -> Eff es String
readFile fc path = send fc (ReadFile path)

writeFile ::
  (e1 :> es) =>
  Send FileSystem e1 ->
  FilePath ->
  String ->
  Eff es ()
writeFile fc path content = send fc (WriteFile path content)

type Effect = (Type -> Type) -> Type -> Type

data E :: Effect where
  Op1 :: E m ()
  Op2 :: E m ()
  Op3 :: E m ()

instance MFunctor E where
  mfmap _ = \case
    Op1 -> Op1
    Op2 -> Op2
    Op3 -> Op3

runE ::
  (e1 :> es) =>
  IOE e1 ->
  (forall e. Send E e -> Eff (e :& es) r) ->
  Eff es r
runE io = interpret $ \case
  Op1 -> effIO io (putStrLn "op1")
  Op2 -> effIO io (putStrLn "op2")
  Op3 -> error "Op3 not implemented"

augmentOp2Interpret ::
  (e1 :> es, e2 :> es) =>
  Send E e1 ->
  IOE e2 ->
  (forall e. Send E e -> Eff (e :& es) r) ->
  Eff es r
augmentOp2Interpret fc io = interpret $ \case
  Op2 -> effIO io (putStrLn "augmented op2") >> send fc Op2
  op -> passthrough fc op

augmentOp2Interpose ::
  (e1 :> es, e2 :> es) =>
  IOE e2 ->
  HandleReader (Send E) e1 ->
  Eff es r ->
  Eff es r
augmentOp2Interpose io = interpose $ \fc -> \case
  Op2 -> effIO io (putStrLn "augmented op2") >> send fc Op2
  op -> passthrough fc op

-- ghci> example
-- op1
-- augmented op2
-- op2
example :: IO ()
example = runEff $ \io -> do
  effIO io (putStrLn "interpret")
  let action fc = send fc Op1 >> send fc Op2
  runE io $ \fc -> augmentOp2Interpret fc io $ \fc' -> action fc'

  effIO io (putStrLn "interpose")
  let actionHandleReader hr = do
        fc <- askHandle hr
        action fc
  runE io $ \fc -> runHandleReader fc $ \hr -> do
    augmentOp2Interpose io hr $ actionHandleReader hr
