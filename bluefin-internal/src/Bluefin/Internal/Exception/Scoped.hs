{-# LANGUAGE RoleAnnotations #-}

module Bluefin.Internal.Exception.Scoped
  ( Exception,
    InFlight,
    try,
    throw,
    newException,
    checkException,
  )
where

import Control.Exception (throwIO, tryJust)
import Control.Exception qualified
import Data.Kind (Type)
import Data.Vault.Strict (Key, Locker, lock, newKey, unlock)

try :: (Exception e -> IO a) -> IO (Either e a)
try k = do
  ex <- newException
  tryJust
    (checkException ex)
    (k ex)

throw :: Exception e -> e -> IO a
throw (MkException key) e = throwIO (MkInFlight (lock key e))

newException :: IO (Exception e)
newException = fmap MkException newKey

-- Corresponds to what Bluefin calls an "Exception", i.e. "a handle to
-- an exception" or "the capability to throw an exception".
newtype Exception (e :: Type) = MkException (Key e)

type role Exception nominal

newtype InFlight = MkInFlight Locker

instance Show InFlight where
  show _ = "In-flight scoped exception"

instance Control.Exception.Exception InFlight

check :: Key a -> InFlight -> Maybe a
check k1 (MkInFlight locker) = unlock k1 locker

checkException :: Exception e -> InFlight -> Maybe e
checkException (MkException key) = check key
