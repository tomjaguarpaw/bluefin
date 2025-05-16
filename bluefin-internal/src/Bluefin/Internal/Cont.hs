{-# LANGUAGE ImpredicativeTypes #-}

module Bluefin.Internal.Cont where

import Bluefin.Internal (Eff, Effects, useImplIn, (:&), (:>))
import Unsafe.Coerce (unsafeCoerce)

newtype EffCont es a = MkEffCont {unEffCont :: forall r. (a -> Eff es r) -> Eff es r}

instance Functor (EffCont es) where
  fmap f m = MkEffCont $ \c -> unEffCont m (c . f)

instance Applicative (EffCont es) where
  pure x = MkEffCont ($ x)
  f <*> v = MkEffCont $ \c -> unEffCont f $ \g -> unEffCont v (c . g)
  m *> k = m >> k

instance Monad (EffCont es) where
  return = pure
  m >>= k = MkEffCont $ \c -> unEffCont m (\x -> unEffCont (k x) c)

data Cont (e :: Effects) = UnsafeMkCont

runCont ::
  (forall e. Cont e -> EffCont (e :& es) a) ->
  Eff es a
runCont f = unsafeRunCont $ \cont -> do
  unEffCont (f cont) return
  where
    unsafeRunCont ::
      (forall e. Cont e -> Eff (e :& es) a) ->
      Eff es a
    unsafeRunCont c = useImplIn c UnsafeMkCont

liftEff ::
  (contE :> es) =>
  Cont contE ->
  Eff es a ->
  EffCont es a
liftEff _ f = MkEffCont $ \effCont -> f >>= effCont

new ::
  Cont contE ->
  ((forall (e :: Effects). f e -> Eff (e :& es) r) -> Eff es r) ->
  EffCont (contE :& es) (f contE)
new f = MkEffCont . forceEffTag f
  where
    forceEffTag ::
      Cont contE ->
      ((forall e. f e -> Eff (e :& es) a) -> Eff es a) ->
      (forall r. (f contE -> Eff (contE :& es) r) -> Eff (contE :& es) r)
    forceEffTag _ = unsafeCoerce
