module Bluefin.Internal.FunctorCoroutine where

import Bluefin.Internal
  ( Eff,
    Handle (mapHandle),
    IOE,
    effIO,
    runEff,
    useImpl,
    useImplIn,
    useImplUnder,
    (:&),
    (:>),
  )
import Data.Kind (Type)

newtype Send f e
  = MkSend (forall e' r. f (Eff e') r -> Eff (e' :& e) r)

instance Handle (Send f) where
  mapHandle (MkSend g) =
    MkSend (useImplUnder . g)

type Effect = (Type -> Type) -> Type -> Type

send ::
  (e1 :> es) =>
  Send f e1 ->
  f (Eff es) r ->
  Eff es r
send (MkSend g) = useImplIn g

type EffectHandler f es =
  forall e r1. f (Eff e) r1 -> Eff (e :& es) r1

interpret ::
  EffectHandler f es ->
  (forall e. Send f e -> Eff (e :& es) r) ->
  Eff es r
interpret g k = useImplIn k (MkSend g)

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

data E :: Effect where
  Op1 :: E m ()
  Op2 :: E m ()
  Op3 :: E m ()

newtype Wrapped f r e = MkWrapped (f (Eff e) r)

class MFunctor f where
  mfmap :: (forall r. m1 r -> m2 r) -> f m1 a -> f m2 a

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

augmentOp2 ::
  (e1 :> es, e2 :> es) =>
  Send E e1 ->
  IOE e2 ->
  (forall e. Send E e -> Eff (e :& es) a) ->
  Eff es a
augmentOp2 fc io = interpret $ \case
  Op2 -> effIO io (putStrLn "augmented op2") >> send fc Op2
  op -> passThrough fc op

passThrough ::
  (MFunctor f, e1 :> es, e2 :> es) =>
  Send f e1 ->
  f (Eff e2) r ->
  Eff es r
passThrough fc = send fc . mfmap useImpl

-- ghci> example
-- op1
-- augmented op2
-- op2
example :: IO ()
example = do
  let action fc = send fc Op1 >> send fc Op2
  runEff $ \io -> runE io $ \fc -> augmentOp2 fc io $ \fc' -> action fc'
