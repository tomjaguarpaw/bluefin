{-# LANGUAGE ImpredicativeTypes #-}

module Bluefin.Internal.Cont where

import Bluefin.Internal (Eff, Effects, useImplIn, (:&))
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

newtype EffCont r m a = MkEffCont {unEffCont :: (a -> m r) -> m r}

instance Functor (EffCont r m) where
  fmap f m = MkEffCont $ \c -> unEffCont m (c . f)

instance Applicative (EffCont r m) where
  pure x = MkEffCont ($ x)
  f <*> v = MkEffCont $ \c -> unEffCont f $ \g -> unEffCont v (c . g)
  m *> k = m >> k

instance Monad (EffCont r m) where
  return = pure
  m >>= k = MkEffCont $ \c -> unEffCont m (\x -> unEffCont (k x) c)

data Cont (e :: Effects) = UnsafeMkCont

runCont ::
  (forall e. Cont e -> EffCont a (Eff (e :& es)) a) ->
  Eff es a
runCont f = unsafeRunCont $ \cont -> do
  unEffCont (f cont) return
  where
    unsafeRunCont ::
      (forall e. Cont e -> Eff (e :& es) a) ->
      Eff es a
    unsafeRunCont c = useImplIn c UnsafeMkCont

liftEff ::
  Cont contE ->
  Eff es a ->
  EffCont r (Eff es) a
liftEff _ f = MkEffCont $ \cont -> f >>= cont

new ::
  Cont contE ->
  ((forall (e :: Effects). f e -> Eff (e :& es) r) -> Eff es r) ->
  EffCont r (Eff (contE :& es)) (f contE)
new f = MkEffCont . forceEffTag f
  where
    forceEffTag ::
      Cont contE ->
      ((forall e. f e -> Eff (e :& es) r) -> Eff es r) ->
      ((f contE -> Eff (contE :& es) r) -> Eff (contE :& es) r)
    forceEffTag _ = unsafeCoerce
