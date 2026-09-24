{-# LANGUAGE DerivingStrategies #-}

module Bluefin.Examples.MonadError
  ( MonadErrorDslBuilder,
    runMonadErrorDslBuilder,
    monadErrorExample,
  )
where

import Bluefin.Capability.ThrowCatch (ThrowCatch)
import Bluefin.Capability.ThrowCatch qualified as ThrowCatch
import Bluefin.Compound (mapHandle)
import Bluefin.DslBuilderEff
  ( DslBuilderEff,
    dslBuilderEff,
    runDslBuilderEff,
    runDslBuilderEffMappedArgs,
  )
import Bluefin.Eff (Eff, runPureEff, type (<:))
import Control.Monad.Except (MonadError (..))

newtype MonadErrorDslBuilder ex es a = MkMonadErrorDslBuilder
  { unMonadErrorDslBuilder :: DslBuilderEff (ThrowCatch ex) es a
  }
  deriving newtype (Functor, Applicative, Monad)

instance MonadError ex (MonadErrorDslBuilder ex es) where
  throwError ex =
    MkMonadErrorDslBuilder $ dslBuilderEff $ \h -> do
      ThrowCatch.throw h ex

  catchError (MkMonadErrorDslBuilder action) handler =
    MkMonadErrorDslBuilder $ dslBuilderEff $ \h -> do
      ThrowCatch.localCatch
        h
        (runDslBuilderEffMappedArgs h action)
        (\ex -> runDslBuilderEffMappedArgs h (unMonadErrorDslBuilder (handler ex)))

runMonadErrorDslBuilder ::
  (e1 <: es) =>
  ThrowCatch ex e1 ->
  MonadErrorDslBuilder ex es a ->
  Eff es a
runMonadErrorDslBuilder h (MkMonadErrorDslBuilder action) =
  runDslBuilderEff (mapHandle h) action

monadErrorExample :: Either String Int
monadErrorExample = runPureEff $ ThrowCatch.try $ \h -> do
  runMonadErrorDslBuilder h $ do
    catchError
      (throwError "could not compute a value")
      (\_ -> pure 42)
