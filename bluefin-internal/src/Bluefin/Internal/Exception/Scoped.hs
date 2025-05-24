module Bluefin.Internal.Exception.Scoped
  ( Exception,
    try,
    throw,
  )
where

import Bluefin.Internal.Key (Key, eqKey, newKey)
import Control.Exception (throwIO, tryJust)
import qualified Control.Exception
import Data.Type.Equality ((:~~:) (HRefl))

try :: (Exception e -> IO a) -> IO (Either e a)
try k = do
  key <- newKey
  tryJust
    (check key)
    (k (MkException key))

throw :: Exception e -> e -> IO a
throw ex e = throwIO (MkInFlight ex e)

newtype Exception e = MkException (Key e)

data InFlight = forall e. MkInFlight !(Exception e) !e

instance Show InFlight where
  show _ = "In-flight scoped exception"

instance Control.Exception.Exception InFlight

check :: Key a -> InFlight -> Maybe a
check k1 (MkInFlight (MkException k2) e) = fmap (\HRefl -> e) (k1 `eqKey` k2)
