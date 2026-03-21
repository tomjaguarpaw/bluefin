{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Bluefin.Internal.DslBuilderEff where

import Bluefin.Internal
import Bluefin.Internal.OneWayCoercible
  ( OneWayCoercible,
    oneWayCoerce,
    oneWayCoercible,
    oneWayCoercibleImpl,
  )

newtype DslBuilderEff h es r
  = MkDslBuilderEff {unMkDslBuilderEff :: forall e. h e -> Eff (e :& es) r}

useImplDslBuilderEff ::
  (e :> es) =>
  DslBuilderEff h e r ->
  -- | ͘
  DslBuilderEff h es r
useImplDslBuilderEff = oneWayCoerce

runDslBuilderEff ::
  h es ->
  DslBuilderEff h es r ->
  -- | ͘
  Eff es r
runDslBuilderEff h f = makeOp (unMkDslBuilderEff f h)

dslBuilderEff ::
  (forall e. h e -> Eff (e :& es) r) ->
  -- | ͘
  DslBuilderEff h es r
dslBuilderEff = MkDslBuilderEff

instance
  (e :> es) =>
  OneWayCoercible (DslBuilderEff h e r) (DslBuilderEff h es r)
  where
  oneWayCoercibleImpl = oneWayCoercible

instance (Handle h) => Functor (DslBuilderEff h es) where
  fmap f g =
    dslBuilderEff $ \h ->
      fmap f (runDslBuilderEff (mapHandle h) (useImplDslBuilderEff g))

instance (Handle h) => Applicative (DslBuilderEff h es) where
  pure x = dslBuilderEff (pure (pure x))
  f <*> x = dslBuilderEff $ \h ->
    runDslBuilderEff (mapHandle h) (useImplDslBuilderEff f)
      <*> runDslBuilderEff (mapHandle h) (useImplDslBuilderEff x)

instance (Handle h) => Monad (DslBuilderEff h es) where
  m >>= f = dslBuilderEff $ \h -> do
    r <- runDslBuilderEff (mapHandle h) (useImplDslBuilderEff m)
    runDslBuilderEff (mapHandle h) (useImplDslBuilderEff (f r))
