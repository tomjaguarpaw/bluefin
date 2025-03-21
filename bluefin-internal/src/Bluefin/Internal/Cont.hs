{-# LANGUAGE ImpredicativeTypes #-}

module Bluefin.Internal.Cont where

import Bluefin.Eff ((:>))
import Bluefin.Internal (Eff, Effects, unsafeUnEff, useImplIn, (:&))
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
runCont f = unsafeRunCont $ \eff -> do
  unEffCont (f eff) return
  where
    unsafeRunCont ::
      (forall e. Cont e -> Eff (e :& es) a) ->
      Eff es a
    unsafeRunCont c = useImplIn c UnsafeMkCont

liftEff ::
  Cont newE ->
  Eff es a ->
  EffCont r (Eff es) a
liftEff _ f = MkEffCont $ \cont -> f >>= cont

new ::
  Cont newE ->
  ((forall (e :: Effects). f e -> Eff (e :& es) r) -> Eff es r) ->
  EffCont r (Eff (newE :& es)) (f newE)
new f = MkEffCont . forceEffTag f
  where
    forceEffTag ::
      Cont newE ->
      ((forall e. f e -> Eff (e :& es) r) -> Eff es r) ->
      ((f1 newE -> Eff (newE :& es) r) -> Eff (newE :& es) r)
    forceEffTag _ = unsafeCoerce
