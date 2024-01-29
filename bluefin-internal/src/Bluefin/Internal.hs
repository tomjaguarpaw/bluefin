{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_HADDOCK not-home #-}

module Bluefin.Internal where

import Control.Exception (throwIO, tryJust)
import qualified Control.Exception
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Unique
import Data.Void (Void, absurd)
import GHC.Exts (Proxy#, proxy#)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (drop, read, return)

type Effect = ()

-- TODO: Rename branch?  Or just use :& directly?
data Effects = Union Effects Effects

-- | Union of effects
type (:&) = Union

newtype Eff (es :: Effects) a = Eff {unsafeUnEff :: IO a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad)

unsafeRemoveEff :: Eff (e :& es) a -> Eff es a
unsafeRemoveEff = Eff . unsafeUnEff

runEff :: (forall es. Eff es a) -> a
runEff e = unsafePerformIO (unsafeUnEff e)

weakenEff :: t `In` t' -> Eff t r -> Eff t' r
weakenEff _ = Eff . unsafeUnEff

insertSecond :: Eff (c1 :& b) r -> Eff (c1 :& (c2 :& b)) r
insertSecond = weakenEff (b (drop (eq (# #))))

assoc1Eff :: Eff ((a :& b) :& c) r -> Eff (a :& (b :& c)) r
assoc1Eff = weakenEff (assoc1 (# #))

newtype Exception e (ex :: Effects) = Exception (forall a. e -> IO a)

newtype State s (st :: Effects) = State (IORef s)

newtype Coroutine a b (s :: Effects) = Coroutine (a -> IO b)

type Stream a (s :: Effects) = Coroutine a () s

newtype In (a :: Effects) (b :: Effects) = In# (# #)

eq :: (# #) -> a `In` a
eq (# #) = In# (# #)

fstI :: (# #) -> a `In` (a :& b)
fstI (# #) = In# (# #)

sndI :: (# #) -> a `In` (b :& a)
sndI (# #) = In# (# #)

cmp :: a `In` b -> b `In` c -> a `In` c
cmp (In# (# #)) (In# (# #)) = In# (# #)

bimap :: a `In` b -> c `In` d -> (a :& c) `In` (b :& d)
bimap (In# (# #)) (In# (# #)) = In# (# #)

assoc1 :: (# #) -> ((a :& b) :& c) `In` (a :& (b :& c))
assoc1 (# #) = In# (# #)

drop :: a `In` b -> a `In` (c :& b)
drop h = w2 (b h)

here :: a `In` b -> (a `In` (b :& c))
here h = w (b2 h)

w :: (a :& b) `In` c -> (a `In` c)
w = cmp (fstI (# #))

w2 :: (b :& a) `In` c -> (a `In` c)
w2 = cmp (sndI (# #))

b2 :: (a `In` b) -> ((a :& c) `In` (b :& c))
b2 h = bimap h (eq (# #))

b :: (a `In` b) -> (c :& a) `In` (c :& b)
b = bimap (eq (# #))

-- | Effect subset constraint
class (effs1 :: Effects) :> (effs2 :: Effects)

-- | A set of effects @e@ is a subset of itself
instance {-# INCOHERENT #-} e :> e

-- | If @e@ is subset of @es@ then @e@ is a subset of a larger set, @x
-- :& es@
instance (e :> es) => e :> (x :& es)

-- Do we want this?
-- instance {-# incoherent #-} (e :> es) => (e' :& e) :> (e' :> es)

-- This seems a bit wobbly

-- | @e@ is a subset of a larger set @e :& es@
instance {-# INCOHERENT #-} e :> (e :& es)

throw ::
  (ex :> effs) =>
  Exception e ex ->
  -- | Value to throw
  e ->
  Eff effs a
throw (Exception throw_) e = Eff (throw_ e)

has :: forall a b. (a :> b) => a `In` b
has = In# (# #)

data Dict c where
  Dict :: forall c. (c) => Dict c

-- Seems like it could be better
have :: forall a b. a `In` b -> Dict (a :> b)
have = unsafeCoerce (Dict @(a :> (a :& b)))

handleException ::
  forall e (effs :: Effects) a.
  -- | ͘
  (forall ex. Exception e ex -> Eff (ex :& effs) a) ->
  -- | ͘
  Eff effs (Either e a)
handleException f =
  Eff $ withScopedException_ (\throw_ -> unsafeUnEff (f (Exception throw_)))

read ::
  (st :> effs) =>
  State s st ->
  -- | ͘
  Eff effs s
read (State r) = Eff (readIORef r)

write ::
  (st :> effs) =>
  State s st ->
  s ->
  -- | ͘
  Eff effs ()
write (State r) !s = Eff (writeIORef r s)

modify ::
  (st :> effs) =>
  State s st ->
  (s -> s) ->
  -- | ͘
  Eff effs ()
modify state f = do
  s <- read state
  write state (f s)

-- This is roughly how effectful does it
data MyException where
  MyException :: e -> Data.Unique.Unique -> MyException

instance Show MyException where
  show _ = "<MyException>"

instance Control.Exception.Exception MyException

withScopedException_ :: ((forall a. e -> IO a) -> IO r) -> IO (Either e r)
withScopedException_ f = do
  fresh <- Data.Unique.newUnique

  flip tryJust (f (\e -> throwIO (MyException e fresh))) $ \case
    MyException e tag ->
      -- unsafeCoerce is very unpleasant
      if tag == fresh then Just (unsafeCoerce e) else Nothing

handleState ::
  s ->
  (forall st. State s st -> Eff (st :& effs) a) ->
  Eff effs (a, s)
handleState s f = do
  state <- Eff (fmap State (newIORef s))
  unsafeRemoveEff $ do
    a <- f state
    s' <- read state
    pure (a, s')

yieldCoroutine ::
  (eff :> effs) =>
  Coroutine a b eff ->
  -- | ͘
  a ->
  Eff effs b
yieldCoroutine (Coroutine f) a = Eff (f a)

yield ::
  (eff :> effs) =>
  Stream a eff ->
  a ->
  -- | ͘
  Eff effs ()
yield = yieldCoroutine

handleCoroutine ::
  (a -> Eff effs b) ->
  (z -> Eff effs r) ->
  (forall eff. Coroutine a b eff -> Eff (eff :& effs) z) ->
  Eff effs r
handleCoroutine update finish f = do
  z <- forEach f update
  finish z

forEach ::
  (forall eff. Coroutine a b eff -> Eff (eff :& effs) r) ->
  -- | ͘
  (a -> Eff effs b) ->
  Eff effs r
forEach f h = unsafeRemoveEff (f (Coroutine (unsafeUnEff . h)))

handleException' ::
  (e -> r) ->
  (forall ex. Exception e ex -> Eff (ex :& effs) r) ->
  Eff effs r
handleException' h f = do
  r1 <- handleException f
  pure $ case r1 of
    Right r -> r
    Left l -> h l

type EarlyReturn = Exception

newtype MustReturnEarly = MustReturnEarly Void

returnedEarly :: MustReturnEarly -> a
returnedEarly (MustReturnEarly v) = absurd v

withEarlyReturn ::
  (forall ex. EarlyReturn r ex -> Eff (ex :& effs) MustReturnEarly) ->
  Eff effs r
withEarlyReturn f = handleException' id (fmap returnedEarly . f)

earlyReturn :: (ex :> effs) => EarlyReturn r ex -> r -> Eff effs a
earlyReturn = throw

evalState ::
  -- | Initial state
  s ->
  (forall st. State s st -> Eff (st :& effs) a) ->
  Eff effs a
evalState s f = fmap fst (handleState s f)

data Compound e1 e2 ss where
  Compound ::
    Proxy# s1 ->
    Proxy# s2 ->
    e1 s1 ->
    e2 s2 ->
    Compound e1 e2 (s1 :& s2)

compound ::
  e1 ex ->
  -- | ͘
  e2 st ->
  Compound e1 e2 (ex :& st)
compound = Compound proxy# proxy#

inComp :: forall a b c r. (a :> b) => (b :> c) => ((a :> c) => r) -> r
inComp k = case have (cmp (has @a @b) (has @b @c)) of Dict -> k

withC ::
  forall e1 e2 ss es r.
  (ss :> es) =>
  Compound e1 e2 ss ->
  -- | ͘
  (forall s1 s2. (s1 :> es, s2 :> es) => e1 s1 -> e2 s2 -> Eff es r) ->
  Eff es r
withC c f =
  case c of
    Compound (_ :: Proxy# st) (_ :: Proxy# st') h i ->
      inComp @st @ss @es (inComp @st' @ss @es (f h i))

withC1 ::
  forall e1 e2 ss es r.
  (ss :> es) =>
  Compound e1 e2 ss ->
  (forall st. (st :> es) => e1 st -> Eff es r) ->
  Eff es r
withC1 c f = withC c (\h _ -> f h)

withC2 ::
  forall e1 e2 ss es r.
  (ss :> es) =>
  Compound e1 e2 ss ->
  (forall st. (st :> es) => e2 st -> Eff es r) ->
  Eff es r
withC2 c f = withC c (\_ i -> f i)

putC :: forall ss es e. (ss :> es) => Compound e (State Int) ss -> Int -> Eff es ()
putC c i = withC2 c (\h -> write h i)

getC :: forall ss es e. (ss :> es) => Compound e (State Int) ss -> Eff es Int
getC c = withC2 c (\h -> read h)

-- TODO: Make this (s1 :> es, s2 :> es), like withC
runC0 ::
  e1 s1 ->
  -- | ͘
  e2 s2 ->
  (forall ss. Compound e1 e2 ss -> Eff (ss :& es) r) ->
  Eff (s1 :& (s2 :& es)) r
runC0 e1 e2 k = assoc1Eff (k (compound e1 e2))

yieldToList ::
  (forall eff. Stream a eff -> Eff (eff :& effs) r) ->
  Eff effs ([a], r)
yieldToList f = yieldToList' (insertSecond . f)

yieldToList' ::
  (forall eff st. Stream a eff -> Eff (eff :& (st :& effs)) r) ->
  Eff effs ([a], r)
yieldToList' f = do
  evalState [] $ \(s :: State lo st) -> do
    r <- forEach f $ \i ->
      modify s (i :)
    as <- read s
    pure (reverse as, r)

type Jump = Exception ()

withJump ::
  (forall j. Jump j -> Eff (j :& effs) ()) ->
  -- | ͘
  Eff effs ()
withJump f = do
  r <- handleException $ \e ->
    f e
  pure (either id id r)

jumpTo ::
  (j :> effs) =>
  Jump j ->
  -- | ͘
  Eff effs a
jumpTo tag = throw tag ()
