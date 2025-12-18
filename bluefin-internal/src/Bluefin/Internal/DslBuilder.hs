{-# LANGUAGE QuantifiedConstraints #-}

module Bluefin.Internal.DslBuilder where

import Bluefin.Internal
import Bluefin.Internal.DslBuilderEffects
  ( DslBuilderEffects,
    dslBuilderEffects,
    runDslBuilderEffects,
  )

newtype Forall f r = MkForall {unForall :: forall es. f es r}

mkForall :: (forall es. f es r) -> Forall f r
mkForall = MkForall

instance (forall es. Functor (f es)) => Functor (Forall f) where
  fmap f k = MkForall (fmap f (unForall k))

instance (forall es. Applicative (f es)) => Applicative (Forall f) where
  pure x = mkForall (pure x)
  f <*> x = mkForall (unForall f <*> unForall x)

instance (forall es. Monad (f es)) => Monad (Forall f) where
  m >>= f = mkForall $ do
    r <- unForall m
    unForall (f r)

newtype DslBuilder h r
  = MkDslBuilder {unMkDslBuilder :: Forall (DslBuilderEffects h) r}

runDslBuilder :: (Handle h) => h es -> DslBuilder h r -> Eff es r
runDslBuilder h f = runDslBuilderEffects h (unForall (unMkDslBuilder f))

dslBuilder :: (forall e. h e -> Eff e r) -> DslBuilder h r
dslBuilder k = MkDslBuilder (mkForall (dslBuilderEffects (useImpl . k)))

instance (Handle h) => Functor (DslBuilder h) where
  fmap f g = dslBuilder (\h -> fmap f (runDslBuilder h g))

instance (Handle h) => Applicative (DslBuilder h) where
  pure x = dslBuilder (pure (pure x))
  f <*> x = dslBuilder (\h -> runDslBuilder h f <*> runDslBuilder h x)

instance (Handle h) => Monad (DslBuilder h) where
  m >>= f = dslBuilder $ \h -> do
    r <- runDslBuilder h m
    runDslBuilder h (f r)
