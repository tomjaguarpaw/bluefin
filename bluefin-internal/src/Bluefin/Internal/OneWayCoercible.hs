{-# OPTIONS_HADDOCK not-home #-}

module Bluefin.Internal.OneWayCoercible
  ( module Bluefin.Internal.OneWayCoercible,
    Generic,
  )
where

import Data.Coerce (Coercible, coerce)
import Data.Type.Coercion (Coercion (Coercion))
import GHC.Generics
import Unsafe.Coerce (unsafeCoerce)

gOneWayCoercion ::
  forall a b. (GOneWayCoercible (Rep a) (Rep b)) => OneWayCoercion a b
gOneWayCoercion = unsafeOneWayCoercion

gOneWayCoercible ::
  (GOneWayCoercible (Rep (h e)) (Rep (h es))) =>
  -- | ͘
  OneWayCoercibleD (h e) (h es)
gOneWayCoercible = MkOneWayCoercibleD gOneWayCoercion

gOneWayCoercible2 ::
  (GOneWayCoercible (Rep (h e e')) (Rep (h es es'))) =>
  OneWayCoercibleD (h e e') (h es es')
gOneWayCoercible2 = MkOneWayCoercibleD gOneWayCoercion

oneWayCoercible :: (Coercible a b) => OneWayCoercibleD a b
oneWayCoercible = MkOneWayCoercibleD (MkOneWayCoercion Coercion)

unsafeOneWayCoercion :: forall a b. OneWayCoercion a b
unsafeOneWayCoercion = MkOneWayCoercion (unsafeCoerce (Coercion @a @a))

unsafeOneWayCoercible :: forall a b. OneWayCoercibleD a b
unsafeOneWayCoercible = MkOneWayCoercibleD unsafeOneWayCoercion

newtype OneWayCoercion a b = MkOneWayCoercion (Coercion a b)

newtype OneWayCoercibleD a b = MkOneWayCoercibleD (OneWayCoercion a b)

oneWayCoercion :: (OneWayCoercible a b) => OneWayCoercion a b
oneWayCoercion = case oneWayCoercibleImpl of
  MkOneWayCoercibleD oneWay -> oneWay

oneWayCoerce :: forall a b. (OneWayCoercible a b) => a -> b
oneWayCoerce = oneWayCoerceWith (oneWayCoercion @a @b)

oneWayCoerceWith :: OneWayCoercion a b -> a -> b
oneWayCoerceWith (MkOneWayCoercion Coercion) = coerce

unsafeCoercionOfOneWayCoercion :: OneWayCoercion a b -> Coercion a b
unsafeCoercionOfOneWayCoercion (MkOneWayCoercion c) = c

unsafeCoercionOfOneWayCoercible ::
  forall a b. (OneWayCoercible a b) => Coercion a b
unsafeCoercionOfOneWayCoercible = case oneWayCoercion @a @b of
  MkOneWayCoercion c -> c

class OneWayCoercible a b where
  oneWayCoercibleImpl :: OneWayCoercibleD a b

instance {-# INCOHERENT #-} OneWayCoercible s s where
  oneWayCoercibleImpl = oneWayCoercible

instance OneWayCoercible () () where
  oneWayCoercibleImpl = oneWayCoercible

instance
  (OneWayCoercible a1 a2) =>
  OneWayCoercible (Maybe a1) (Maybe a2)
  where
  oneWayCoercibleImpl = case unsafeCoercionOfOneWayCoercible @a1 @a2 of
    Coercion -> oneWayCoercible

instance
  (OneWayCoercible a1 a2, OneWayCoercible b1 b2) =>
  OneWayCoercible (Either a1 b1) (Either a2 b2)
  where
  oneWayCoercibleImpl = case unsafeCoercionOfOneWayCoercible @a1 @a2 of
    Coercion -> case unsafeCoercionOfOneWayCoercible @b1 @b2 of
      Coercion -> oneWayCoercible

-- | Other sizes of tuples follow this pattern. We will add them when
-- someone needs them.
instance
  (OneWayCoercible a1 a2, OneWayCoercible b1 b2) =>
  OneWayCoercible (a1, b1) (a2, b2)
  where
  oneWayCoercibleImpl = case unsafeCoercionOfOneWayCoercible @a1 @a2 of
    Coercion -> case unsafeCoercionOfOneWayCoercible @b1 @b2 of
      Coercion -> oneWayCoercible

instance
  (OneWayCoercible a1 a2, OneWayCoercible b1 b2, OneWayCoercible c1 c2) =>
  OneWayCoercible (a1, b1, c1) (a2, b2, c2)
  where
  oneWayCoercibleImpl = case unsafeCoercionOfOneWayCoercible @a1 @a2 of
    Coercion -> case unsafeCoercionOfOneWayCoercible @b1 @b2 of
      Coercion -> case unsafeCoercionOfOneWayCoercible @c1 @c2 of
        Coercion -> oneWayCoercible

trans :: OneWayCoercion a b -> OneWayCoercion b c -> OneWayCoercion a c
trans c1 c2 = case unsafeCoercionOfOneWayCoercion c1 of
  Coercion -> case unsafeCoercionOfOneWayCoercion c2 of
    Coercion -> MkOneWayCoercion Coercion

class GOneWayCoercible a b

instance GOneWayCoercible U1 U1

instance
  (OneWayCoercible c c') =>
  GOneWayCoercible (K1 i c) (K1 i' c')

instance
  (GOneWayCoercible f f') =>
  GOneWayCoercible (M1 i t f) (M1 i' t' f')

instance
  (GOneWayCoercible f f', GOneWayCoercible g g') =>
  GOneWayCoercible (f :*: g) (f' :*: g')

instance
  (OneWayCoercible a a', OneWayCoercible b b') =>
  OneWayCoercible (a' -> b) (a -> b')
  where
  oneWayCoercibleImpl = case oneWayCoercion @a @a' of
    MkOneWayCoercion Coercion -> case oneWayCoercion @b @b' of
      MkOneWayCoercion Coercion ->
        MkOneWayCoercibleD (MkOneWayCoercion Coercion)
