{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Bluefin.Examples.CapabilityAttenuation where

import Bluefin.Capability.Modify qualified as Static
import Bluefin.Compound
  ( Handle,
    OneWayCoercible (oneWayCoercibleImpl),
    OneWayCoercibleHandle (..),
    mapHandle,
    oneWayCoercibleTrustMe,
    useImpl,
    useImplIn,
  )
import Bluefin.Eff (Eff, runPureEff, type (:&), type (<:))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | A simple lens, defined locally to avoid depending on a lens library.
type Lens' b a = forall f. Functor f => (a -> f a) -> b -> f b

view :: Lens' b a -> b -> a
view lens = getConst . lens Const

set :: Lens' b a -> a -> b -> b
set lens value = runIdentity . lens (const (Identity value))

over :: Lens' b a -> (a -> a) -> b -> b
over lens f = runIdentity . lens (Identity . f)

-- | A modify capability whose implementation is supplied dynamically.
data DynamicModify s e = DynamicModify
  { getImpl :: Eff e s,
    putImpl :: s -> Eff e (),
    modifyImpl :: (s -> s) -> Eff e ()
  }
  deriving (Handle) via OneWayCoercibleHandle (DynamicModify s)

instance
  (e <: es) =>
  OneWayCoercible (DynamicModify s e) (DynamicModify s es)
  where
  oneWayCoercibleImpl = oneWayCoercibleTrustMe $ \mo ->
    DynamicModify
      { getImpl = useImpl (getImpl mo),
        putImpl = useImpl . putImpl mo,
        modifyImpl = useImpl . modifyImpl mo
      }

get :: (e <: es) => DynamicModify s e -> Eff es s
get handle = getImpl (mapHandle handle)

put :: (e <: es) => DynamicModify s e -> s -> Eff es ()
put handle = putImpl (mapHandle handle)

modify :: (e <: es) => DynamicModify s e -> (s -> s) -> Eff es ()
modify handle = modifyImpl (mapHandle handle)

toDynamic ::
  (e1 <: es) =>
  Static.Modify s e1 ->
  (forall e. DynamicModify s e -> Eff (e :& es) r) ->
  Eff es r
toDynamic handle body =
  useImplIn
    body
    DynamicModify
      { getImpl = Static.get handle,
        putImpl = Static.put handle,
        modifyImpl = Static.modify handle
      }

-- | Attenuate a modify capability to the part selected by a lens.
attenuateByLens ::
  (e1 <: es) =>
  Lens' s a ->
  DynamicModify s e1 ->
  (forall e. DynamicModify a e -> Eff (e :& es) r) ->
  Eff es r
attenuateByLens lens handle body =
  useImplIn
    body
    DynamicModify
      { getImpl = view lens <$> get handle,
        putImpl = \value -> modify handle (set lens value),
        modifyImpl = modify handle . over lens
      }

-- | A capability that can write, but cannot read, a value.
data WriteOnly s e = WriteOnly
  { writeImpl :: s -> Eff e ()
  }
  deriving (Handle) via OneWayCoercibleHandle (WriteOnly s)

instance
  (e <: es) =>
  OneWayCoercible (WriteOnly s e) (WriteOnly s es)
  where
  oneWayCoercibleImpl = oneWayCoercibleTrustMe $ \writeHandle ->
    WriteOnly
      { writeImpl = useImpl . writeImpl writeHandle
      }

write :: (e <: es) => WriteOnly s e -> s -> Eff es ()
write handle = writeImpl (mapHandle handle)

attenuateModifyToWrite ::
  (e1 <: es) =>
  DynamicModify s e1 ->
  (forall e. WriteOnly s e -> Eff (e :& es) r) ->
  Eff es r
attenuateModifyToWrite handle body =
  useImplIn
    body
    WriteOnly
      { writeImpl = put handle
      }

-- >>> example
-- fromList [("good bye","C++"),("hello","world")]
example :: Map String String
example = runPureEff $ do
  let initial =
        Map.fromList
          [ ("hello", "kephas"),
            ("good bye", "C++")
          ]
  Static.evalModify initial $ \keyValues -> do
    -- `keyValues` lets us modify the whole map.
    toDynamic keyValues $ \dynamicMap ->
      -- Attenuate `dynamicMap` from whole-map access to just the "hello" value.
      attenuateByLens (flip Map.alterF "hello") dynamicMap $ \helloValue ->
        -- Attenuate `helloValue` from read/write access to write-only `helloValueWrite`.
        attenuateModifyToWrite helloValue $ \helloValueWrite ->
          -- `helloValueWrite` lets us set it without reading its old value.
          write helloValueWrite (Just "world")

    Static.get keyValues
