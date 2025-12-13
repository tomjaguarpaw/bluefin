{-# LANGUAGE DerivingVia #-}

module Bluefin.Internal.GadtEffect where

import Bluefin.Internal
  ( Eff,
    Effects,
    Handle,
    HandleReader,
    OneWayCoercibleHandle (..),
    localHandle,
    mapHandle,
    oneWayCoercibleTrustMe,
    useImplIn,
    useImplUnder,
    (:&),
    (:>),
  )
import Bluefin.Internal.OneWayCoercible (OneWayCoercible (oneWayCoercibleImpl), OneWayCoercibleD)
import Data.Kind (Type)

type Send :: Effect -> Effects -> Type
-- | Bring a 'Send' into scope with 'interpret'.
newtype Send f e = MkSend (EffectHandler f e)
  deriving (Handle) via OneWayCoercibleHandle (Send f)

-- | A convenient type synoynm matching
-- [@effectful@](https://hackage-content.haskell.org/package/effectful-core/docs/Effectful.html#t:Effect)
-- and
-- [@polysemy@](https://hackage.haskell.org/package/polysemy/docs/Polysemy.html#t:Effect)'s
-- usages provided for people who are migrating from those libraries.
type Effect = (Type -> Type) -> Type -> Type

instance (e :> es) => OneWayCoercible (Send f e) (Send f es) where
  oneWayCoercibleImpl =
    oneWayCoercibleTrustMe (\(MkSend g) -> MkSend (useImplUnder . g))

-- | Send a primitive operation to the handler for interpretation.
-- This is the Bluefin analog of @effectful@'s
-- [@send@](https://hackage.haskell.org/package/effectful-core/docs/Effectful-Dispatch-Dynamic.html#v:send)
-- and @polysemy@'s
-- [@send@](https://hackage.haskell.org/package/polysemy/docs/Polysemy.html#v:send).
send ::
  (e1 :> es) =>
  Send f e1 ->
  -- | Handle this operation using the effect handler currently in
  -- scope for the @Send f@ handle.
  f (Eff es) r ->
  Eff es r
send (MkSend g) = useImplIn g

-- | A convenient type synonym.  This is like @effectful@'s
-- [@EffectHandler@](https://hackage-content.haskell.org/package/effectful-core-2.6.1.0/docs/Effectful-Dispatch-Dynamic.html#t:EffectHandler).
-- A similar type also appears in @polysemy@ as the argument to
-- functions like
-- [@intercept@](https://hackage.haskell.org/package/polysemy-1.9.2.0/docs/Polysemy.html#v:intercept).
type EffectHandler f es =
  forall e r.
  f (Eff e) r ->
  -- | ͘
  Eff (e :& es) r

-- |
-- @
-- import System.IO qualified as IO
--
-- runFileSystem ::
--   forall es e1 e2 r.
--   (e1 :> es, e2 :> es) =>
--   t'Bluefin.IO.IOE' e1 ->
--   t'Bluefin.Exception.Exception' t'Control.Exception.IOException' e2 ->
--   (forall e. 'Send' FileSystem e -> Eff (e :& es) r) ->
--   Eff es r
-- runFileSystem io ex = 'interpret' $ \\case
--   ReadFile path ->
--     adapt (IO.'System.IO.readFile' path)
--   WriteFile path contents ->
--     adapt (IO.'System.IO.writeFile' path contents)
--   Trace msg body -> do
--     'Bluefin.IO.effIO' io (putStrLn ("Start: " <> msg))
--     r <- 'Bluefin.Compound.useImpl' body
--     effIO io (putStrLn ("End: " <> msg))
--     pure r
--   where
--     -- If you don't want to write this signature you can use
--     -- {-# LANGUAGE NoMonoLocalBinds #-}
--     adapt :: (e1 :> es', e2 :> es') => IO r' -> Eff es' r'
--     adapt m = 'Bluefin.IO.rethrowIO' io ex (effIO io m)
-- @
interpret ::
  -- | Implementation of effect handler for @Send f@
  EffectHandler f es ->
  -- | Within this block, @send@ has the implementation given above.
  (forall e. Send f e -> Eff (e :& es) r) ->
  Eff es r
interpret g k = useImplIn k (MkSend g)

type GadtEffect :: ((Type -> Type) -> Type -> Type) -> Type -> Effects -> Type
newtype GadtEffect f a e = MkGadtEffect {unGadtEffect :: f (Eff e) a}

mapGadtEffect ::
  (f1 (Eff e1) r1 -> f2 (Eff e2) r2) ->
  GadtEffect f1 r1 e1 ->
  GadtEffect f2 r2 e2
mapGadtEffect f = MkGadtEffect . f . unGadtEffect

-- |
-- @
-- instance
--   (e :> es) =>
--   t'Bluefin.Compound.OneWayCoercible' ('GadtEffect' FileSystem r e) (GadtEffect FileSystem r es)
--   where
--   'Bluefin.Compound.oneWayCoercibleImpl' = 'oneWayCoercibleGadtEffectTrustMe' $ \\case
--     ReadFile path -> ReadFile path
--     WriteFile path contents -> WriteFile path contents
--     Trace msg body -> Trace msg (useImpl body)
-- @
oneWayCoercibleGadtEffectTrustMe ::
  (e :> es) =>
  (forall e' es'. (e' :> es') => f (Eff e') r -> f (Eff es') r) ->
  -- | ͘
  OneWayCoercibleD (GadtEffect f r e) (GadtEffect f r es)
oneWayCoercibleGadtEffectTrustMe k = oneWayCoercibleTrustMe (mapGadtEffect k)

-- | Version of 'send' for use when pattern matching in 'interpose'
--
-- @
-- augmentOp2Interpose ::
--   (e1 :> es, e2 :> es) =>
--   IOE e2 ->
--   t'Bluefin.HandleReader.HandleReader' (Send E) e1 ->
--   Eff es r ->
--   Eff es r
-- augmentOp2Interpose io = 'interpose' $ \\fc -> \\case
--   Op2 -> effIO io (putStrLn "augmented op2") >> send fc Op2
--   op -> 'passthrough' fc op
-- @
passthrough ::
  (Handle (GadtEffect f r), e1 :> es, e2 :> es) =>
  Send f e1 ->
  f (Eff e2) r ->
  -- | ͘
  Eff es r
passthrough fc = send fc . unGadtEffect . mapHandle . MkGadtEffect

-- |
-- @
-- augmentOp2Interpose ::
--   (e1 :> es, e2 :> es) =>
--   IOE e2 ->
--   t'Bluefin.HandleReader.HandleReader' (Send E) e1 ->
--   Eff es r ->
--   Eff es r
-- augmentOp2Interpose io = 'interpose' $ \\fc -> \\case
--   Op2 -> effIO io (putStrLn "augmented op2") >> send fc Op2
--   op -> 'passthrough' fc op
-- @
interpose ::
  (e1 :> es) =>
  -- | Reimplementation of effect handler for @Send f@ in terms of the
  -- the original effect handler, which is passed as the argument
  (Send f es -> EffectHandler f es) ->
  -- | Original effect handler
  HandleReader (Send f) e1 ->
  -- | Within this block, @send@ has the implementation given above.
  Eff es r ->
  Eff es r
interpose h hr = localHandle hr (\fcOrig -> MkSend (h fcOrig))
