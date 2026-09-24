{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Bluefin.Internal.Capability.ThrowCatch
  ( ThrowCatch,
    module Bluefin.Internal.Capability.ThrowCatch,
  )
where

import Bluefin.Internal
  ( ThrowCatch (MkThrowCatch),
    Eff,
    throwCatchTry,
    throwCatchThrow,
    unsafeProvideIO,
    useImpl,
    withEffToIO_,
    (:&),
    type (<:),
  )
import Bluefin.Internal.Exception.Scoped qualified as Scoped

try ::
  (forall e. ThrowCatch ex e -> Eff (e :& es) a) ->
  -- | ͘
  Eff es (Either ex a)
try = throwCatchTry

handle ::
  (ex -> Eff es a) ->
  (forall e. ThrowCatch ex e -> Eff (e :& es) a) ->
  -- | ͘
  Eff es a
handle h f =
  try f >>= \case
    Left ex -> h ex
    Right a -> pure a

catch ::
  (forall e. ThrowCatch ex e -> Eff (e :& es) a) ->
  (ex -> Eff es a) ->
  -- | ͘
  Eff es a
catch f h = handle h f

localCatch ::
  (e <: es) =>
  ThrowCatch ex e ->
  Eff es a ->
  (ex -> Eff es a) ->
  -- | ͘
  Eff es a
localCatch h action handler =
  localTry h action >>= \case
    Left ex -> handler ex
    Right a -> pure a

localHandle ::
  (e <: es) =>
  ThrowCatch ex e ->
  (ex -> Eff es a) ->
  Eff es a ->
  -- | ͘
  Eff es a
localHandle h handler action = localCatch h action handler

throw ::
  (e <: es) =>
  ThrowCatch ex e ->
  ex ->
  -- | ͘
  Eff es a
throw = throwCatchThrow

localTry ::
  (e <: es) =>
  ThrowCatch ex e ->
  Eff es a ->
  -- | ͘
  Eff es (Either ex a)
localTry h action = case h of
  MkThrowCatch ex ->
    unsafeProvideIO $ \io ->
      withEffToIO_ io $ \runInIO ->
        Scoped.localTry ex (runInIO (useImpl action))
