module Bluefin.Internal.Effectful where

import Bluefin.Internal
import qualified Effectful
import Effectful.Internal.Env
import qualified Effectful.Internal.Monad as Effectful
import qualified Effectful.State.Dynamic as St

newtype Effectful es (e :: Effects) = MkEffectful (Env es)

useEffectful ::
  (e :> es) => Effectful effes e -> Effectful.Eff effes r -> Eff es r
useEffectful (MkEffectful env) k = UnsafeMkEff (Effectful.unEff k env)

handleWith ::
  (e1 :> es) =>
  (Effectful.Eff (effe : effes) r1 -> Effectful.Eff effes r2) ->
  (forall e. Effectful (effe : effes) e -> Eff (e :& es) r1) ->
  Effectful effes e1 ->
  Eff es r2
handleWith handler m (MkEffectful env) =
  UnsafeMkEff
    ( Effectful.unEff
        ( handler
            (Effectful.unsafeEff (\env' -> unsafeUnEff (m (MkEffectful env'))))
        )
        env
    )

runEffectful ::
  (e1 :> es) =>
  IOE e1 ->
  (forall e. Effectful '[Effectful.IOE] e -> Eff (e :& es) r) ->
  Eff e1 r
runEffectful ioe k =
  effIO ioe $ Effectful.runEff (Effectful.unsafeEff (\env -> unsafeUnEff (k (MkEffectful env))))

runPureEffectful ::
  (forall e. Effectful '[] e -> Eff (e :& es) r) ->
  Eff e1 r
runPureEffectful k =
  pure (Effectful.runPureEff (Effectful.unsafeEff (\env -> unsafeUnEff (k (MkEffectful env)))))


example :: (St.State Int Effectful.:> es) => Effectful.Eff es Int
example = do
  r <- St.get
  St.put (r + 1 :: Int)
  St.get

bfExample ::
  (e :> es, St.State Int Effectful.:> effes) =>
  Effectful effes e ->
  Eff es Int
bfExample e = useEffectful e example

runExample :: Int
runExample = runPureEff (runPureEffectful (handleWith (St.evalStateLocal (10 :: Int)) bfExample))
