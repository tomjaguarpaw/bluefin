-- Like Key from vault:
--
-- https://github.com/HeinrichApfelmus/vault/blob/master/src/Data/Vault/ST/backends/GHC.h#L19C29-L20C1
--
-- and Tag from prim-uniq:
--
-- https://hackage.haskell.org/package/prim-uniq-0.2/docs/Data-Unique-Tag.html
{-# LANGUAGE RoleAnnotations #-}

module Bluefin.Internal.Key
  ( Key,
    newKey,
    eqKey,
  )
where

import Data.Coerce (coerce)
import Data.Type.Equality ((:~~:) (HRefl))
import Data.Unique (Unique, newUnique)
import Unsafe.Coerce (unsafeCoerce)

type role Key nominal

newtype Key (a :: k) = MkKey Unique

newKey :: IO (Key a)
newKey = coerce newUnique

-- 'Key' cannot lawfully be 'TestEquality', though it could be 'GEq'
-- and 'GOrd'.
eqKey :: forall a b. Key a -> Key b -> Maybe (a :~~: b)
eqKey (MkKey id1) (MkKey id2) =
  if id1 == id2
    then Just (unsafeCoerce (HRefl @a))
    else Nothing
