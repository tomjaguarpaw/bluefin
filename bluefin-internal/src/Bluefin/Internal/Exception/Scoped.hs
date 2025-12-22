module Bluefin.Internal.Exception.Scoped
  ( Exception,
    try,
    throw,
    newException,
    checkException,
    InFlight,
  )
where

import Bluefin.Internal.Key (Key, eqKey, newKey)
import Control.Exception (throwIO, tryJust)
import Control.Exception qualified
import Data.Kind (Type)
import Data.Type.Equality ((:~~:) (HRefl))

try :: (Exception e -> IO a) -> IO (Either e a)
try k = do
  ex <- newException
  tryJust
    (checkException ex)
    (k ex)

throw :: Exception e -> e -> IO a
throw ex e = throwIO (MkInFlight ex e)

newException :: IO (Exception e)
newException = do
  key <- newKey
  pure (MkException key)

-- Corresponds to what Bluefin calls an "Exception", i.e. "a handle to
-- an exception" or "the capability to throw an exception".
newtype Exception (e :: Type) = MkException (Key e)

-- InFlight is like Locker from vault.  MkInflight is like lock from
-- vault.
data InFlight = forall e. MkInFlight !(Exception e) !e

instance Show InFlight where
  show _ = "In-flight scoped exception"

instance Control.Exception.Exception InFlight

-- Like unlock from vault
checkException :: Exception a -> InFlight -> Maybe a
checkException (MkException k) = check k

-- Like unlock from vault
check :: Key a -> InFlight -> Maybe a
check k1 (MkInFlight (MkException k2) e) = fmap (\HRefl -> e) (k1 `eqKey` k2)
