module Bluefin.Internal.Effectful where

import Bluefin.Internal
import Control.Monad (when)
import qualified Effectful
import qualified Effectful.Error.Dynamic as Er
import Effectful.Internal.Env
import qualified Effectful.Internal.Monad as Effectful
import qualified Effectful.State.Dynamic as St

newtype Effectful es (e :: Effects) = MkEffectful (Env es)

data Bluefin es m a

useEffectful ::
  (e :> es) =>
  -- | Bluefin handle to @effectful@ operations
  Effectful effes e ->
  -- | An @effectful@ operation
  Effectful.Eff effes r ->
  Eff es r
useEffectful (MkEffectful env) k = UnsafeMkEff (Effectful.unEff k env)

toEffectful ::
  Bluefin es Effectful.:> effes =>
  (forall e. Effectful effes e -> Eff (e :& es) a) ->
  Effectful.Eff effes a
toEffectful = _

fromEffectful ::
  e :> es =>
  Effectful.Eff (Bluefin es : effes) r ->
  Effectful effes e ->
  Eff es r
fromEffectful = _

unsafeToEffectful :: (Effectful es e -> Eff es' a) -> Effectful.Eff es a
unsafeToEffectful m =
  Effectful.unsafeEff (\env' -> unsafeUnEff (m (MkEffectful env')))

handleWith ::
  (e1 :> es) =>
  -- | An @effectful@ handler
  (Effectful.Eff (effe : effes) r1 -> Effectful.Eff effes r2) ->
  -- | An @effectful@ operation, in Bluefin style
  (forall e. Effectful (effe : effes) e -> Eff (e :& es) r1) ->
  Effectful effes e1 ->
  Eff es r2
handleWith handler m (MkEffectful env) =
  UnsafeMkEff (Effectful.unEff (handler (unsafeToEffectful m)) env)

runEffectful ::
  (e1 :> es) =>
  IOE e1 ->
  -- | An @effectful@ operation, in Bluefin style (with IO)
  (forall e. Effectful '[Effectful.IOE] e -> Eff (e :& es) r) ->
  Eff e1 r
runEffectful ioe k = effIO ioe (Effectful.runEff (unsafeToEffectful k))

runPureEffectful ::
  -- | An @effectful@ operation, in Bluefin style (without IO)
  (forall e. Effectful '[] e -> Eff (e :& es) r) ->
  Eff es r
runPureEffectful k = pure (Effectful.runPureEff (unsafeToEffectful k))

example ::
  (St.State Int Effectful.:> es, Er.Error String Effectful.:> es) =>
  Effectful.Eff es Int
example = do
  r <- St.get
  St.put (r + 1 :: Int)
  r' <- St.get @Int
  when (r' > 10) (Er.throwError "foo")
  St.get

bfExample ::
  ( e :> es,
    e1 :> es,
    St.State Int Effectful.:> effes,
    Er.Error String Effectful.:> effes
  ) =>
  State Int e1 ->
  Effectful effes e ->
  Eff es Int
bfExample s e = do
  r <- useEffectful e example
  put s r
  pure r

runExample :: Int -> Either String Int
runExample i =
  runPureEff
    ( evalState 1000 $ \s ->
        runPureEffectful
          ( handleWith
              Er.runErrorNoCallStack
              (handleWith (St.evalStateLocal i) (bfExample s))
          )
    )

-- > runExample 9
-- Right 10
-- > runExample 10
-- Left "foo"
