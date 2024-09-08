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

-- * Bluefin handle

-- | Provide access to Effectful operations in Bluefin
newtype Effectful (es' :: [EffectfulEffect]) (e :: Effects)
  = MkEffectful (Env es')

-- * Effectful effect

-- | Provide access to Bluefin operations in Effectful
data Bluefin es m a

-- * Type synonyms

type instance DispatchOf (Bluefin es) = Dynamic

type EffectfulEff = Effectful.Eff

type EffectfulEffect = Effectful.Effect

type a ::> b = a Effectful.:> b

-- * Bluefin handlers

runEffectful ::
  (e1 :> es) =>
  IOE e1 ->
  -- | An @effectful@ operation, in Bluefin style (with IO)
  (forall e. Effectful '[Effectful.IOE] e -> Eff (e :& es) r) ->
  Eff es r
runEffectful ioe k = effIO ioe (Effectful.runEff (unsafeToEffectful k))

runPureEffectful ::
  -- | An @effectful@ operation, in Bluefin style (without IO)
  (forall e. Effectful '[] e -> Eff (e :& es) r) ->
  Eff es r
runPureEffectful k = pure (Effectful.runPureEff (unsafeToEffectful k))

handleWith ::
  (e1 :> es) =>
  -- | An @effectful@ handler
  (EffectfulEff (e' : es') r1 -> EffectfulEff es' r2) ->
  -- | An @effectful@ operation, in Bluefin style
  (forall e. Effectful (e' : es') e -> Eff (e :& es) r1) ->
  Effectful es' e1 ->
  -- | The result of handling the @effectful@ operation in Bluefin
  -- style using the @effectful@ handler
  Eff es r2
handleWith handler m (MkEffectful env) =
  UnsafeMkEff (Effectful.unEff (handler (unsafeToEffectful m)) env)

-- * Effectful handlers

runBluefin ::
  (Effectful.IOE ::> es') =>
  -- | A Bluefin operation, in Effectful style (with IO)
  (forall e es. IOE e -> EffectfulEff (Bluefin (e :& es) : es') r) ->
  EffectfulEff es' r
runBluefin m = unsafeInterpretBluefin (m MkIOE)

runPureBluefin ::
  -- | A Bluefin operation, in Effectful style (without IO)
  (forall es. EffectfulEff (Bluefin es : es') r) ->
  EffectfulEff es' r
runPureBluefin = unsafeInterpretBluefin

-- * Use Bluefin operations in @effectful@ and vice versa

useEffectful ::
  (e :> es) =>
  -- | Bluefin handle to @effectful@ operations
  Effectful es' e ->
  -- | An @effectful@ operation
  EffectfulEff es' r ->
  Eff es r
useEffectful e k =
  fromEffectful (\Proxy -> Effectful.inject k) e

useBluefin ::
  forall es es' r.
  (Bluefin es ::> es') =>
  Eff es r ->
  -- | ͘
  EffectfulEff es' r
useBluefin m =
  toEffectful (\(_ :: Effectful es' e) -> useImpl @es @(e :& es) m)

-- * Conversion between @effectful@ and Bluefin

toEffectful ::
  forall (es :: Effects) (es' :: [EffectfulEffect]) a.
  (Bluefin es ::> es') =>
  (forall e. Effectful es' e -> Eff (e :& es) a) ->
  -- | ͘
  EffectfulEff es' a
toEffectful = unsafeToEffectful

fromEffectful ::
  (e :> es) =>
  (Proxy es -> EffectfulEff (Bluefin es : es') r) ->
  Effectful es' e ->
  -- | ͘
  Eff es r
fromEffectful m (MkEffectful env) =
  UnsafeMkEff (Effectful.unEff (unsafeInterpretBluefin (m Proxy)) env)

-- * Example code

example ::
  ( St.State Int ::> es',
    Er.Error String ::> es',
    Bluefin es ::> es',
    e :> es
  ) =>
  State Int e ->
  Proxy es ->
  -- | ͘
  EffectfulEff es' Int
example bst (_ :: Proxy bes) = do
  r <- St.get
  St.put (r + 1 :: Int)
  r' <- St.get @Int
  when (r' > 10) (Er.throwError "foo")
  useBluefin @bes (put bst r')
  St.get

bfExample ::
  forall e es e1 es'.
  ( e :> es,
    e1 :> es,
    St.State Int ::> es',
    Er.Error String ::> es'
  ) =>
  State Int e1 ->
  Effectful es' e ->
  -- | ͘
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

-- * Unsafe internals

voidBluefin :: Bluefin es m a -> r
voidBluefin = \case {}

unsafeInterpretBluefin ::
  EffectfulEff (Bluefin es : es') a -> EffectfulEff es' a
unsafeInterpretBluefin = Effectful.interpret (\_ -> voidBluefin)

unsafeToEffectful :: (Effectful es e -> Eff es' a) -> EffectfulEff es a
unsafeToEffectful m =
  Effectful.unsafeEff (\env' -> unsafeUnEff (m (MkEffectful env')))
