{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Bluefin.Internal.DslBuilderEffects where

import Bluefin.Internal
import Bluefin.Internal.OneWayCoercible
  ( OneWayCoercible,
    oneWayCoercible,
    oneWayCoercibleImpl,
  )

newtype DslBuilderEffects h es r
  = MkDslBuilderEffects {unMkDslBuilderEffects :: forall e. h e -> Eff (e :& es) r}

useImplDslBuilderEffects :: (e :> es) => DslBuilderEffects h e r -> DslBuilderEffects h es r
useImplDslBuilderEffects (MkDslBuilderEffects f) = MkDslBuilderEffects (useImplUnder . f)

runDslBuilderEffects :: h es -> DslBuilderEffects h es r -> Eff es r
runDslBuilderEffects h f = makeOp (unMkDslBuilderEffects f h)

dslBuilderEffects :: (forall e. h e -> Eff (e :& es) r) -> DslBuilderEffects h es r
dslBuilderEffects = MkDslBuilderEffects

instance
  (e :> es) =>
  OneWayCoercible (DslBuilderEffects h e r) (DslBuilderEffects h es r)
  where
  oneWayCoercibleImpl = oneWayCoercible

instance (Handle h) => Functor (DslBuilderEffects h es) where
  fmap f g =
    dslBuilderEffects $ \h ->
      fmap f (runDslBuilderEffects (mapHandle h) (useImplDslBuilderEffects g))

instance (Handle h) => Applicative (DslBuilderEffects h es) where
  pure x = dslBuilderEffects (pure (pure x))
  f <*> x = dslBuilderEffects $ \h ->
    runDslBuilderEffects (mapHandle h) (useImplDslBuilderEffects f)
      <*> runDslBuilderEffects (mapHandle h) (useImplDslBuilderEffects x)

instance (Handle h) => Monad (DslBuilderEffects h es) where
  m >>= f = dslBuilderEffects $ \h -> do
    r <- runDslBuilderEffects (mapHandle h) (useImplDslBuilderEffects m)
    runDslBuilderEffects (mapHandle h) (useImplDslBuilderEffects (f r))
