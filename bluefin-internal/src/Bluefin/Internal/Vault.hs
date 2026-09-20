{-# LANGUAGE RoleAnnotations #-}

module Bluefin.Internal.Vault
  ( Key,
    newKey,
    lookup,
    insert,
    adjust,
    delete,
    Vault,
    Vault.empty,
  )
where

import Data.Vault.Strict as Vault
  ( Key,
    Vault,
    adjust,
    delete,
    empty,
    insert,
    lookup,
    newKey,
  )
import Prelude ()
