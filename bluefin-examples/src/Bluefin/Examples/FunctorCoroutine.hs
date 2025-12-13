module Bluefin.Examples.FunctorCoroutine where

import Bluefin.Compound (Handle, handleImpl, useImpl)
import Bluefin.Eff (Eff, runEff, (:&), (:>))
import Bluefin.Exception (Exception, rethrowIO)
import Bluefin.FunctorCoroutine
  ( Flip,
    Send,
    handleFlip,
    interpose,
    interpret,
    passthrough,
    send,
  )
import Bluefin.HandleReader (HandleReader, asksHandle, runHandleReader)
import Bluefin.IO (IOE, effIO)
import Control.Exception (IOException)
import Data.Kind (Type)
import System.IO qualified as IO

data FileSystem :: Effect where
  ReadFile :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()
  Trace :: String -> m r -> FileSystem m r

instance Handle (Flip FileSystem r) where
  handleImpl = handleFlip $ \case
    ReadFile path -> ReadFile path
    WriteFile path contents -> WriteFile path contents
    Trace msg body -> Trace msg (useImpl body)

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

trace ::
  (e1 :> es) =>
  Send FileSystem e1 ->
  String ->
  Eff es r ->
  Eff es r
trace fc msg body = send fc (Trace msg body)

runFileSystem ::
  forall es e1 e2 r.
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception IOException e2 ->
  (forall e. Send FileSystem e -> Eff (e :& es) r) ->
  Eff es r
runFileSystem io ex = interpret $ \case
  ReadFile path -> adapt (IO.readFile path)
  WriteFile path contents -> adapt (IO.writeFile path contents)
  Trace msg body -> do
    effIO io (putStrLn ("Start: " <> msg))
    r <- useImpl body
    effIO io (putStrLn ("End: " <> msg))
    pure r
  where
    -- If you don't want to write this signature you can use
    -- {-# LANGUAGE NoMonoLocalBinds #-}
    adapt :: (e1 :> es', e2 :> es') => IO r' -> Eff es' r'
    adapt m = rethrowIO io ex (effIO io m)

type Effect = (Type -> Type) -> Type -> Type

data E :: Effect where
  Op1 :: E m ()
  Op2 :: E m ()
  Op3 :: E m ()

instance Handle (Flip E r) where
  handleImpl = handleFlip $ \case
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
  Op3 -> effIO io (putStrLn "op3")

augmentOp2Interpret ::
  (e1 :> es, e2 :> es) =>
  IOE e2 ->
  Send E e1 ->
  (forall e. Send E e -> Eff (e :& es) r) ->
  Eff es r
augmentOp2Interpret io fc = interpret $ \case
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
-- -- interpret:
-- op1
-- augmented op2
-- op2
-- op3
-- -- interpose:
-- op1
-- augmented op2
-- op2
-- op3
example :: IO ()
example = runEff $ \io -> do
  let action fc = do
        send fc Op1
        send fc Op2
        send fc Op3

  effIO io (putStrLn "-- interpret:")
  runE io $ \fc -> do
    augmentOp2Interpret io fc $ \fc' -> action fc'

  effIO io (putStrLn "-- interpose:")
  runE io $ \fc -> runHandleReader fc $ \hr -> do
    augmentOp2Interpose io hr $ asksHandle hr action
