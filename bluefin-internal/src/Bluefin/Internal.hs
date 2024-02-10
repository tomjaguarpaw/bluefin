{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_HADDOCK not-home #-}

module Bluefin.Internal where

import Control.Exception (throwIO, tryJust)
import qualified Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Unique
import GHC.Exts (Proxy#, proxy#)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (drop, read, return)

type Effect = ()

-- TODO: Rename branch?  Or just use :& directly?
data Effects = Union Effects Effects

-- | Union of effects
infixr 9 :&

type (:&) = Union

newtype Eff (es :: Effects) a = Eff {unsafeUnEff :: IO a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad)

newtype EffReaderT r m a = MkEffReaderT {unEffReaderT :: r -> m a}
  deriving (Functor)

instance (Applicative m) => Applicative (EffReaderT r m) where
  pure = MkEffReaderT . pure . pure
  MkEffReaderT f <*> MkEffReaderT x = MkEffReaderT (\r -> f r <*> x r)

instance (Monad m) => Monad (EffReaderT r m) where
  MkEffReaderT m >>= f =
    MkEffReaderT (\r -> m r >>= (\a -> unEffReaderT (f a) r))

instance (e :> effs) => MonadIO (EffReaderT (IOE e) (Eff effs)) where
  liftIO = MkEffReaderT . flip effIO

withMonadIO :: IOE e -> EffReaderT (IOE e) (Eff effs) r -> Eff effs r
withMonadIO = flip unEffReaderT

unsafeRemoveEff :: Eff (e :& es) a -> Eff es a
unsafeRemoveEff = Eff . unsafeUnEff

-- | Run an 'Eff' that doesn't contain any unhandled effects.
runEff :: (forall es. Eff es a) -> a
runEff e = unsafePerformIO (unsafeUnEff e)

weakenEff :: t `In` t' -> Eff t r -> Eff t' r
weakenEff _ = Eff . unsafeUnEff

insertSecond :: Eff (c1 :& b) r -> Eff (c1 :& (c2 :& b)) r
insertSecond = weakenEff (b (drop (eq (# #))))

assoc1Eff :: Eff ((a :& b) :& c) r -> Eff (a :& (b :& c)) r
assoc1Eff = weakenEff (assoc1 (# #))

-- | Handle to an exception of type @e@
newtype Exception e (ex :: Effects) = Exception (forall a. e -> IO a)

-- | A handle to a state of type @a@
newtype State s (st :: Effects) = State (IORef s)

-- | A handle to a coroutine that expects values of type @a@ and then
-- yields values of type @b@.
newtype Coroutine a b (s :: Effects) = Coroutine (a -> IO b)

-- | A handle to a stream that yields values of type @a@.  It is
-- implemented as a handle to a coroutine that expects values of type
-- @()@ and then yields values of type @a@.
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

-- |
-- @
-- >>> runEff $ try $ \\e -> do
--       throw e 42
--       pure "No exception thrown"
-- Left 42
-- @
--
-- @
-- >>> runEff $ try $ \\e -> do
--       pure "No exception thrown"
-- Right "No exception thrown"
-- @
throw ::
  (ex :> effs) =>
  Exception e ex ->
  -- | Value to throw
  e ->
  Eff effs a
throw (Exception throw_) e = Eff (throw_ e)

throwExample :: Either Int String
throwExample = runEff $ try $ \e -> do
  _ <- throw e 42
  pure "No exception thrown"

has :: forall a b. (a :> b) => a `In` b
has = In# (# #)

data Dict c where
  Dict :: forall c. (c) => Dict c

-- Seems like it could be better
have :: forall a b. a `In` b -> Dict (a :> b)
have = unsafeCoerce (Dict @(a :> (a :& b)))

-- |
-- @
-- >>> runEff $ try $ \\e -> do
--       throw e 42
--       pure "No exception thrown"
-- Left 42
-- @
try ::
  forall e (effs :: Effects) a.
  (forall ex. Exception e ex -> Eff (ex :& effs) a) ->
  -- | @Left@ if the exception was thrown, @Right@ otherwise
  Eff effs (Either e a)
try f =
  Eff $ withScopedException_ (\throw_ -> unsafeUnEff (f (Exception throw_)))

-- | 'handle', but with the argument order swapped
--
-- @
-- >>> runEff $ handle (pure . show) $ \\e -> do
--       throw e 42
--       pure "No exception thrown"
-- "42"
-- @
handle ::
  forall e (effs :: Effects) a.
  -- | If the exception is thrown, apply this handler
  (e -> Eff effs a) ->
  (forall ex. Exception e ex -> Eff (ex :& effs) a) ->
  Eff effs a
handle h f =
  try f >>= \case
    Left e -> h e
    Right a -> pure a

handleExample :: String
handleExample = runEff $ handle (pure . show) $ \e -> do
  _ <- throw e (42 :: Int)
  pure "No exception thrown"

catch ::
  forall e (effs :: Effects) a.
  (forall ex. Exception e ex -> Eff (ex :& effs) a) ->
  -- | If the exception is thrown, apply this handler
  (e -> Eff effs a) ->
  Eff effs a
catch f h = handle h f

-- |
-- @
-- >>> runEff $ runState 10 $ \\st -> do
--       n <- get st
--       pure (2 * n)
-- (20,10)
-- @
get ::
  (st :> effs) =>
  State s st ->
  -- | The current value of the state
  Eff effs s
get (State r) = Eff (readIORef r)

exampleGet :: (Int, Int)
exampleGet = runEff $ runState 10 $ \st -> do
  n <- get st
  pure (2 * n)

-- | Set the value of the state
--
-- @
-- >>> runEff $ runState 10 $ \\st -> do
--       put st 30
--       pure ()
-- ((), 30)
-- @
put ::
  (st :> effs) =>
  State s st ->
  -- | The new value of the state.  The new value is forced before
  -- writing it to the state.
  s ->
  Eff effs ()
put (State r) !s = Eff (writeIORef r s)

examplePut :: ((), Int)
examplePut = runEff $ runState 10 $ \st -> do
  put st 30
  pure ()

-- |
-- @
-- >>> runEff $ runState 10 $ \\st -> do
--       modify st (* 2)
-- ((), 20)
-- @
modify ::
  (st :> effs) =>
  State s st ->
  -- | Apply this function to the state
  (s -> s) ->
  Eff effs ()
modify state f = do
  s <- get state
  put state (f s)

modifyExample :: ((), Int)
modifyExample = runEff $ runState 10 $ \st -> do
  modify st (* 2)

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

-- |
-- @
-- >>> runEff $ runState 10 $ \\st -> do
--       n <- get st
--       pure (2 * n)
-- (20,10)
-- @
runState ::
  -- | Initial state
  s ->
  -- | Stateful computation
  (forall st. State s st -> Eff (st :& effs) a) ->
  -- | Result and final state
  Eff effs (a, s)
runState s f = do
  state <- Eff (fmap State (newIORef s))
  unsafeRemoveEff $ do
    a <- f state
    s' <- get state
    pure (a, s')

yieldCoroutine ::
  (e1 :> effs) =>
  Coroutine a b e1 ->
  -- | ͘
  a ->
  Eff effs b
yieldCoroutine (Coroutine f) a = Eff (f a)

-- |
-- @
-- >>> runEff $ yieldToList $ \\y -> do
--       yield y 1
--       yield y 2
--       yield y 100
-- ([1,2,100], ())
-- @
yield ::
  (e1 :> effs) =>
  Stream a e1 ->
  -- | Yield this value from the stream
  a ->
  Eff effs ()
yield = yieldCoroutine

yieldExample :: ([Int], ())
yieldExample = runEff $ yieldToList $ \y -> do
  yield y 1
  yield y 2
  yield y 100

handleCoroutine ::
  (a -> Eff effs b) ->
  (z -> Eff effs r) ->
  (forall e1. Coroutine a b e1 -> Eff (e1 :& effs) z) ->
  Eff effs r
handleCoroutine update finish f = do
  z <- forEach f update
  finish z

-- |
-- @
-- >>> runEff $ yieldToList $ \\y -> do
--       forEach (inFoldable [0 .. 3]) $ \\i -> do
--         yield y i
--         yield y (i * 10)
-- ([0, 0, 1, 10, 2, 20, 3, 30], ())
-- @
forEach ::
  (forall e1. Coroutine a b e1 -> Eff (e1 :& effs) r) ->
  -- | Apply this effectful function for each element of the coroutine
  (a -> Eff effs b) ->
  Eff effs r
forEach f h = unsafeRemoveEff (f (Coroutine (unsafeUnEff . h)))

forEachExample :: ([Int], ())
forEachExample = runEff $ yieldToList $ \y -> do
  forEach (inFoldable [0 .. 4]) $ \i -> do
    yield y i
    yield y (i * 10)

-- |
-- @
-- >>> runEff $ yieldToList $ inFoldable [1, 2, 100]
-- ([1, 2, 100], ())
-- @
inFoldable ::
  (Foldable t, e1 :> effs) =>
  -- | Yield all these values from the stream
  t a ->
  Stream a e1 ->
  Eff effs ()
inFoldable t = for_ t . yield

inFoldableExample :: ([Int], ())
inFoldableExample = runEff $ yieldToList $ inFoldable [1, 2, 100]

-- | Pair each element in the stream with an increasing index,
-- starting from 0.
--
-- @
-- >>> runEff $ yieldToList $ enumerate (inFoldable [\"A\", \"B\", \"C\"])
-- ([(0, \"A\"), (1, \"B\"), (2, \"C\")], ())
-- @
enumerate ::
  (e2 :> effs) =>
  -- | ͘
  (forall e1. Stream a e1 -> Eff (e1 :& effs) ()) ->
  Stream (Int, a) e2 ->
  Eff effs ()
enumerate ss st = evalState 0 $ \i -> forEach (insertSecond . ss) $ \s -> do
  ii <- get i
  yield st (ii, s)
  put i (ii + 1)

enumerateExample :: ([(Int, String)], ())
enumerateExample = runEff $ yieldToList $ enumerate (inFoldable ["A", "B", "C"])

type EarlyReturn = Exception

withEarlyReturn ::
  (forall er. EarlyReturn r er -> Eff (er :& effs) r) ->
  -- | ͘
  Eff effs r
withEarlyReturn = handle pure

earlyReturn ::
  (er :> effs) =>
  EarlyReturn r er ->
  r ->
  -- | ͘
  Eff effs a
earlyReturn = throw

-- |
-- @
-- >>> runEff $ evalState 10 $ \\st -> do
--       n <- get st
--       pure (2 * n)
-- 20
-- @
evalState ::
  -- | Initial state
  s ->
  -- | Stateful computation
  (forall st. State s st -> Eff (st :& effs) a) ->
  -- | Result
  Eff effs a
evalState s f = fmap fst (runState s f)

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
putC c i = withC2 c (\h -> put h i)

getC :: forall ss es e. (ss :> es) => Compound e (State Int) ss -> Eff es Int
getC c = withC2 c (\h -> get h)

-- TODO: Make this (s1 :> es, s2 :> es), like withC
runC0 ::
  e1 s1 ->
  -- | ͘
  e2 s2 ->
  (forall ss. Compound e1 e2 ss -> Eff (ss :& es) r) ->
  Eff (s1 :& (s2 :& es)) r
runC0 e1 e2 k = assoc1Eff (k (compound e1 e2))

-- |
-- @
-- >>> runEff $ yieldToList $ \\y -> do
--       yield y 1
--       yield y 2
--       yield y 100
-- ([1,2,100], ())
-- @
yieldToList ::
  (forall e1. Stream a e1 -> Eff (e1 :& effs) r) ->
  -- | Yielded elements and final result
  Eff effs ([a], r)
yieldToList f = do
  (as, r) <- yieldToReverseList f
  pure (reverse as, r)

-- | This is more efficient than 'yieldToList' because it gathers the
-- elements into a stack in reverse order. @yieldToList@ then reverses
-- that stack.
--
-- @
-- >>> runEff $ yieldToReverseList $ \\y -> do
--       yield y 1
--       yield y 2
--       yield y 100
-- ([100,2,1], ())
-- @
yieldToReverseList ::
  (forall e1. Stream a e1 -> Eff (e1 :& effs) r) ->
  -- | Yielded elements in reverse order, and final result
  Eff effs ([a], r)
yieldToReverseList f = do
  evalState [] $ \(s :: State lo st) -> do
    r <- forEach (insertSecond . f) $ \i ->
      modify s (i :)
    as <- get s
    pure (as, r)

type Jump = EarlyReturn ()

withJump ::
  (forall j. Jump j -> Eff (j :& effs) ()) ->
  -- | ͘
  Eff effs ()
withJump = withEarlyReturn

jumpTo ::
  (j :> effs) =>
  Jump j ->
  -- | ͘
  Eff effs a
jumpTo tag = throw tag ()

-- | Handle that allows you to run 'IO' operations
data IOE (e :: Effects) = IOE

-- | Run an 'IO' operation in 'Eff'
--
-- @
-- >>> runEffIO $ \\io -> do
--       effIO io (putStrLn "Hello world!")
-- Hello, world!
-- @
effIO ::
  (e :> effs) =>
  IOE e ->
  IO a ->
  -- | ͘
  Eff effs a
effIO IOE = Eff

effIOExample :: IO ()
effIOExample = runEffIO $ \io -> do
  effIO io (putStrLn "Hello world!")

-- | Run an 'Eff' whose only unhandled effect is 'IO'.
--
-- @
-- >>> runEffIO $ \\io -> do
--       effIO io (putStrLn "Hello world!")
-- Hello, world!
-- @
runEffIO ::
  (forall e effs. IOE e -> Eff (e :& effs) a) ->
  -- | ͘
  IO a
runEffIO eff = unsafeUnEff (eff IOE)
