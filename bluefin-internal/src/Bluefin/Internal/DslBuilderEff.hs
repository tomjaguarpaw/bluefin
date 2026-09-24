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
import GHC.Base (oneShot)
import GHC.IO (IO (IO))

newtype DslBuilderEff h es r
  = MkDslBuilderEff {unMkDslBuilderEff :: forall e. h e -> Eff (e :& es) r}

useImplDslBuilderEff ::
  (e <: es) =>
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
{-# INLINE [0] runDslBuilderEff #-}
-- GHC's simplifier phase numbers count down toward 0. INLINE [0] keeps this
-- wrapper intact until phase 0, the final phase, so earlier simplifications
-- can work with the call before its body is exposed; it then strongly
-- encourages inlining to remove the wrapper. See
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pragmas.html#phase-control

-- oneShot is essential for good performance. I don't fully understand
-- why.

runDslBuilderEffMappedArgs ::
  forall e1 e2 es h r.
  (Handle h, e1 <: es, e2 <: es) =>
  h e1 ->
  DslBuilderEff h e2 r ->
  -- | ͘
  Eff es r
runDslBuilderEffMappedArgs h f =
  runDslBuilderEff (mapHandle h) (useImplDslBuilderEff f)
{-# INLINE runDslBuilderEffMappedArgs #-}

dslBuilderEff ::
  (forall e. h e -> Eff (e :& es) r) ->
  -- | ͘
  DslBuilderEff h es r
dslBuilderEff f = MkDslBuilderEff $ \h -> case f h of
  UnsafeMkEff g -> UnsafeMkEff $ oneShot $ \env -> case g env of
    -- Expose IO's state transformer so it too can be marked one-shot
    IO io -> IO (oneShot io)

instance
  (e <: es) =>
  OneWayCoercible (DslBuilderEff h e r) (DslBuilderEff h es r)
  where
  oneWayCoercibleImpl = oneWayCoercible

instance (Handle h) => Functor (DslBuilderEff h es) where
  fmap f g =
    dslBuilderEff $ \h ->
      fmap f (runDslBuilderEffMappedArgs h g)

instance (Handle h) => Applicative (DslBuilderEff h es) where
  pure x = dslBuilderEff (pure (pure x))
  f <*> x = dslBuilderEff $ \h ->
    runDslBuilderEffMappedArgs h f
      <*> runDslBuilderEffMappedArgs h x

instance (Handle h) => Monad (DslBuilderEff h es) where
  m >>= f = dslBuilderEff $ \h -> do
    r <- runDslBuilderEffMappedArgs h m
    runDslBuilderEffMappedArgs h (f r)
