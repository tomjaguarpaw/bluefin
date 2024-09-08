{-# LANGUAGE TypeFamilies #-}

module Bluefin.Internal.Effectful where

import Bluefin.Internal
import Control.Monad (when)
import Data.Proxy (Proxy (Proxy))
import qualified Effectful
import qualified Effectful.Dispatch.Dynamic as Effectful
import qualified Effectful.Error.Dynamic as Er
import Effectful.Internal.Env
import qualified Effectful.Internal.Monad as Effectful
import qualified Effectful.State.Dynamic as St

newtype Effectful es (e :: Effects) = MkEffectful (Env es)

data Bluefin es m a

type instance DispatchOf (Bluefin es) = Dynamic

voidBluefin :: Bluefin es m a -> r
voidBluefin = \case {}

useEffectful ::
  (e :> es) =>
  -- | Bluefin handle to @effectful@ operations
  Effectful effes e ->
  -- | An @effectful@ operation
  Effectful.Eff effes r ->
  Eff es r
useEffectful e k =
  fromEffectful (\Proxy -> Effectful.inject k) e

useBluefin ::
  forall es effes r.
  (Bluefin es Effectful.:> effes) =>
  Eff es r ->
  Effectful.Eff effes r
useBluefin m = Effectful.unsafeEff (\_ -> unsafeUnEff m)

toEffectful ::
  forall es effes a.
  (Bluefin es Effectful.:> effes) =>
  (forall e. Effectful effes e -> Eff (e :& es) a) ->
  Effectful.Eff effes a
toEffectful = unsafeToEffectful

unsafeInterpretBluefin ::
  Effectful.Eff (Bluefin es : effes) a -> Effectful.Eff effes a
unsafeInterpretBluefin = Effectful.interpret (\_ -> voidBluefin)

fromEffectful ::
  (e :> es) =>
  (Proxy es -> Effectful.Eff (Bluefin es : effes) r) ->
  Effectful effes e ->
  Eff es r
fromEffectful m (MkEffectful env) =
  UnsafeMkEff (Effectful.unEff (unsafeInterpretBluefin (m Proxy)) env)

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
  ( St.State Int Effectful.:> es,
    Er.Error String Effectful.:> es,
    Bluefin bes Effectful.:> es,
    e :> bes
  ) =>
  State Int e ->
  Proxy bes ->
  Effectful.Eff es Int
example bst (_ :: Proxy bes) = do
  r <- St.get
  St.put (r + 1 :: Int)
  r' <- St.get @Int
  when (r' > 10) (Er.throwError "foo")
  useBluefin @bes (put bst r')
  St.get

bfExample ::
  forall e es e1 effes.
  ( e :> es,
    e1 :> es,
    St.State Int Effectful.:> effes,
    Er.Error String Effectful.:> effes
  ) =>
  State Int e1 ->
  Effectful effes e ->
  Eff es Int
bfExample s e = do
  r <- fromEffectful (\_ -> example s (Proxy @es)) e
  put s r
  pure r

runExample :: Int -> Either String Int
runExample i =
  runPureEff $
    evalState 1000 $ \s ->
      runPureEffectful $
        fromEffectful $ \(_ :: Proxy es) ->
          Er.runErrorNoCallStack $
            toEffectful @es $
              fromEffectful $ \(_ :: Proxy es') ->
                St.evalStateLocal i $
                  toEffectful @es' (bfExample s)

-- > runExample 9
-- Right 10
-- > runExample 10
-- Left "foo"
