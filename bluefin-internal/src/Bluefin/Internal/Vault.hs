module Bluefin.Internal.Vault
  ( module Bluefin.Internal.Vault,
    Vault,
    Vault.empty,
  )
where

import Data.Kind (Type)
import Data.Vault.Strict (Vault)
import Data.Vault.Strict qualified as Vault
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

-- Vault.Key doesn't have representational role for its "value"
-- argument. I think it should. We hack it.
--
--     https://github.com/HeinrichApfelmus/vault/issues/56
type Key :: Type -> Type
newtype Key a = MkKey (Vault.Key Any)

fromMine :: Key a -> Vault.Key a
fromMine (MkKey k) = unsafeCoerce k

toMine :: Vault.Key a -> Key a
toMine k = MkKey (unsafeCoerce k)

lookup :: Key a -> Vault -> Maybe a
lookup = Vault.lookup . fromMine

adjust :: (a -> a) -> Key a -> Vault -> Vault
adjust f = Vault.adjust f . fromMine

newKey :: IO (Key a)
newKey = fmap toMine Vault.newKey

insert :: Key a -> a -> Vault -> Vault
insert = Vault.insert . fromMine
