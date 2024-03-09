{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_HADDOCK not-home #-}

module Bluefin.Internal where

import Control.Exception (throwIO, tryJust)
import qualified Control.Exception
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Trans.Control (MonadBaseControl, StM, liftBaseWith, restoreM)
import qualified Control.Monad.Trans.Reader as Reader
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Unique
import GHC.Exts (Proxy#, proxy#)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (drop, head, read, return)

data Effects = Union Effects Effects

-- | @type (:&) :: Effects -> Effects -> Effects@
--
-- Union of effects
infixr 9 :&

type (:&) = Union

newtype Eff (es :: Effects) a = UnsafeMkEff {unsafeUnEff :: IO a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad)

-- | Because doing 'IO' operations inside 'Eff' requires a value-level
-- argument we can't give @IO@-related instances to @Eff@ directly.
-- Instead we wrap it in @EffReader@.
newtype EffReader r es a = MkEffReader {unEffReader :: r -> Eff es a}
  deriving (Functor, Applicative, Monad) via (Reader.ReaderT r (Eff es))

instance (e :> es) => MonadIO (EffReader (IOE e) es) where
  liftIO = MkEffReader . flip effIO

effReader :: (r -> Eff es a) -> EffReader r es a
effReader = MkEffReader

runEffReader :: r -> EffReader r es a -> Eff es a
runEffReader r (MkEffReader m) = m r

-- This is possibly what @withRunInIO@ should morally be.
withEffToIO ::
  (e2 :> es) =>
  -- | Continuation with the unlifting function in scope.
  ((forall r. (forall e1. IOE e1 -> Eff (e1 :& es) r) -> IO r) -> IO a) ->
  IOE e2 ->
  Eff es a
withEffToIO k io = effIO io (k (\f -> unsafeUnEff (f MkIOE)))

-- We don't try to do anything sophisticated here.  I haven't thought
-- through all the consequences.
instance (e :> es) => MonadUnliftIO (EffReader (IOE e) es) where
  withRunInIO ::
    ((forall a. EffReader (IOE e) es a -> IO a) -> IO b) ->
    EffReader (IOE e) es b
  withRunInIO k =
    MkEffReader
      ( UnsafeMkEff
          . Reader.runReaderT
            ( withRunInIO
                ( \f ->
                    k
                      ( f
                          . Reader.ReaderT
                          . (unsafeUnEff .)
                          . unEffReader
                      )
                )
            )
      )

instance (e :> es) => MonadBase IO (EffReader (IOE e) es) where
  liftBase = liftIO

instance (e :> es) => MonadBaseControl IO (EffReader (IOE e) es) where
  type StM (EffReader (IOE e) es) a = a
  liftBaseWith = withRunInIO
  restoreM = pure

instance (e :> es) => MonadFail (EffReader (Exception String e) es) where
  fail = MkEffReader . flip throw

hoistReader ::
  (forall b. m b -> n b) ->
  Reader.ReaderT r m a ->
  Reader.ReaderT r n a
hoistReader f = Reader.ReaderT . (\m -> f . Reader.runReaderT m)

-- | Run `MonadIO` operations in 'Eff'.
--
-- @
-- >>> runEff $ \\io -> withMonadIO io $ liftIO $ do
--       putStrLn "Hello world!"
-- Hello, world!
-- @

-- This is not really any better than just running the action in
-- `IO`.
withMonadIO ::
  (e :> es) =>
  IOE e ->
  -- | 'MonadIO' operation
  (forall m. (MonadIO m) => m r) ->
  -- | @MonadIO@ operation run in @Eff@
  Eff es r
withMonadIO io m = unEffReader m io

-- | Run 'MonadFail' operations in 'Eff'.
--
-- @
-- >>> runPureEff $ try $ \\e ->
--       when (2 > 1) $
--         withMonadFail e (fail "2 was bigger than 1")
-- Left "2 was bigger than 1"
-- @

-- This is not really any better than just running the action in
-- `Either String` and then applying `either (throw f) pure`.
withMonadFail ::
  (e :> es) =>
  -- | @Exception@ to @throw@ on @fail@
  Exception String e ->
  -- | 'MonadFail' operation
  (forall m. (MonadFail m) => m r) ->
  -- | @MonadFail@ operation run in @Eff@
  Eff es r
withMonadFail f m = unEffReader m f

unsafeRemoveEff :: Eff (e :& es) a -> Eff es a
unsafeRemoveEff = UnsafeMkEff . unsafeUnEff

-- | Run an 'Eff' that doesn't contain any unhandled effects.
runPureEff :: (forall es. Eff es a) -> a
runPureEff e = unsafePerformIO (unsafeUnEff e)

weakenEff :: t `In` t' -> Eff t r -> Eff t' r
weakenEff _ = UnsafeMkEff . unsafeUnEff

insertFirst :: Eff b r -> Eff (c1 :& b) r
insertFirst = weakenEff (drop (eq (# #)))

insertSecond :: Eff (c1 :& b) r -> Eff (c1 :& (c2 :& b)) r
insertSecond = weakenEff (b (drop (eq (# #))))

assoc1Eff :: Eff ((a :& b) :& c) r -> Eff (a :& (b :& c)) r
assoc1Eff = weakenEff (assoc1 (# #))

pushFirst :: Eff a r -> Eff (a :& b) r
pushFirst = weakenEff (fstI (# #))

-- | Handle to an exception of type @e@
newtype Exception e (ex :: Effects) = Exception (forall a. e -> IO a)

-- | A handle to a strict mutable state of type @a@
newtype State s (st :: Effects) = UnsafeMkState (IORef s)

-- | A handle to a coroutine that expects values of type @a@ and then
-- yields values of type @b@.
newtype Coroutine a b (s :: Effects) = UnsafeMkCoroutine (a -> IO b)

-- | A handle to a stream that yields values of type @a@.  It is
-- implemented as a handle to a coroutine that expects values of type
-- @()@ and then yields values of type @a@.
type Stream a = Coroutine a ()

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
class (es1 :: Effects) :> (es2 :: Effects)

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
-- >>> runPureEff $ try $ \\e -> do
--       throw e 42
--       pure "No exception thrown"
-- Left 42
-- @
--
-- @
-- >>> runPureEff $ try $ \\e -> do
--       pure "No exception thrown"
-- Right "No exception thrown"
-- @
throw ::
  (ex :> es) =>
  Exception e ex ->
  -- | Value to throw
  e ->
  Eff es a
throw (Exception throw_) e = UnsafeMkEff (throw_ e)

has :: forall a b. (a :> b) => a `In` b
has = In# (# #)

data Dict c where
  Dict :: forall c. (c) => Dict c

-- Seems like it could be better
have :: forall a b. a `In` b -> Dict (a :> b)
have = unsafeCoerce (Dict @(a :> (a :& b)))

-- |
-- @
-- >>> runPureEff $ try $ \\e -> do
--       throw e 42
--       pure "No exception thrown"
-- Left 42
-- @
try ::
  forall e (es :: Effects) a.
  (forall ex. Exception e ex -> Eff (ex :& es) a) ->
  -- | @Left@ if the exception was thrown, @Right@ otherwise
  Eff es (Either e a)
try f =
  UnsafeMkEff $ withScopedException_ (\throw_ -> unsafeUnEff (f (Exception throw_)))

-- | 'handle', but with the argument order swapped
--
-- @
-- >>> runPureEff $ handle (pure . show) $ \\e -> do
--       throw e 42
--       pure "No exception thrown"
-- "42"
-- @
handle ::
  forall e (es :: Effects) a.
  -- | If the exception is thrown, apply this handler
  (e -> Eff es a) ->
  (forall ex. Exception e ex -> Eff (ex :& es) a) ->
  Eff es a
handle h f =
  try f >>= \case
    Left e -> h e
    Right a -> pure a

catch ::
  forall e (es :: Effects) a.
  (forall ex. Exception e ex -> Eff (ex :& es) a) ->
  -- | If the exception is thrown, apply this handler
  (e -> Eff es a) ->
  Eff es a
catch f h = handle h f

-- |
-- @
-- >>> runPureEff $ runState 10 $ \\st -> do
--       n <- get st
--       pure (2 * n)
-- (20,10)
-- @
get ::
  (st :> es) =>
  State s st ->
  -- | The current value of the state
  Eff es s
get (UnsafeMkState r) = UnsafeMkEff (readIORef r)

-- | Set the value of the state
--
-- @
-- >>> runPureEff $ runState 10 $ \\st -> do
--       put st 30
-- ((), 30)
-- @
put ::
  (st :> es) =>
  State s st ->
  -- | The new value of the state.  The new value is forced before
  -- writing it to the state.
  s ->
  Eff es ()
put (UnsafeMkState r) s = UnsafeMkEff (writeIORef r $! s)

-- |
-- @
-- >>> runPureEff $ runState 10 $ \\st -> do
--       modify st (* 2)
-- ((), 20)
-- @
modify ::
  (st :> es) =>
  State s st ->
  -- | Apply this function to the state.  The new value of the state
  -- is forced before writing it to the state.
  (s -> s) ->
  Eff es ()
modify state f = do
  s <- get state
  put state (f s)

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
-- >>> runPureEff $ runState 10 $ \\st -> do
--       n <- get st
--       pure (2 * n)
-- (20,10)
-- @
runState ::
  -- | Initial state
  s ->
  -- | Stateful computation
  (forall st. State s st -> Eff (st :& es) a) ->
  -- | Result and final state
  Eff es (a, s)
runState s f = do
  state <- UnsafeMkEff (fmap UnsafeMkState (newIORef s))
  unsafeRemoveEff $ do
    a <- f state
    s' <- get state
    pure (a, s')

yieldCoroutine ::
  (e1 :> es) =>
  Coroutine a b e1 ->
  -- | ͘
  a ->
  Eff es b
yieldCoroutine (UnsafeMkCoroutine f) a = UnsafeMkEff (f a)

-- |
-- @
-- >>> runPureEff $ yieldToList $ \\y -> do
--       yield y 1
--       yield y 2
--       yield y 100
-- ([1,2,100], ())
-- @
yield ::
  (e1 :> es) =>
  Stream a e1 ->
  -- | Yield this value from the stream
  a ->
  Eff es ()
yield = yieldCoroutine

handleCoroutine ::
  (a -> Eff es b) ->
  (z -> Eff es r) ->
  (forall e1. Coroutine a b e1 -> Eff (e1 :& es) z) ->
  Eff es r
handleCoroutine update finish f = do
  z <- forEach f update
  finish z

-- |
-- @
-- >>> runPureEff $ yieldToList $ \\y -> do
--       forEach (inFoldable [0 .. 3]) $ \\i -> do
--         yield y i
--         yield y (i * 10)
-- ([0, 0, 1, 10, 2, 20, 3, 30], ())
-- @
forEach ::
  (forall e1. Coroutine a b e1 -> Eff (e1 :& es) r) ->
  -- | Apply this effectful function for each element of the coroutine
  (a -> Eff es b) ->
  Eff es r
forEach f h = unsafeRemoveEff (f (UnsafeMkCoroutine (unsafeUnEff . h)))

-- |
-- @
-- >>> runPureEff $ yieldToList $ inFoldable [1, 2, 100]
-- ([1, 2, 100], ())
-- @
inFoldable ::
  (Foldable t, e1 :> es) =>
  -- | Yield all these values from the stream
  t a ->
  Stream a e1 ->
  Eff es ()
inFoldable t = for_ t . yield

-- | Pair each element in the stream with an increasing index,
-- starting from 0.
--
-- @
-- >>> runPureEff $ yieldToList $ enumerate (inFoldable [\"A\", \"B\", \"C\"])
-- ([(0, \"A\"), (1, \"B\"), (2, \"C\")], ())
-- @
enumerate ::
  (e2 :> es) =>
  -- | ͘
  (forall e1. Stream a e1 -> Eff (e1 :& es) r) ->
  Stream (Int, a) e2 ->
  Eff es r
enumerate s = enumerateFrom 0 s

-- | Pair each element in the stream with an increasing index,
-- starting from an inital value.
--
-- @
-- >>> runPureEff $ yieldToList $ enumerateFrom1 (inFoldable [\"A\", \"B\", \"C\"])
-- ([(1, \"A\"), (2, \"B\"), (3, \"C\")], ())
-- @
enumerateFrom ::
  (e2 :> es) =>
  -- | Initial value
  Int ->
  (forall e1. Stream a e1 -> Eff (e1 :& es) r) ->
  Stream (Int, a) e2 ->
  Eff es r
enumerateFrom n ss st =
  evalState n $ \i -> forEach (insertSecond . ss) $ \s -> do
    ii <- get i
    yield st (ii, s)
    put i (ii + 1)

type EarlyReturn = Exception

-- | Run an 'Eff' action with the ability to return early to this
-- point.  In the language of exceptions, 'withEarlyReturn' installs
-- an exception handler for an exception of type @r@.
--
-- @
-- >>> runPureEff $ withEarlyReturn $ \\e -> do
--       for_ [1 .. 10] $ \\i -> do
--         when (i >= 5) $
--           returnEarly e ("Returned early with " ++ show i)
--       pure "End of loop"
-- "Returned early with 5"
-- @
withEarlyReturn ::
  (forall er. EarlyReturn r er -> Eff (er :& es) r) ->
  -- | ͘
  Eff es r
withEarlyReturn = handle pure

-- |
-- @
-- >>> runPureEff $ withEarlyReturn $ \\e -> do
--       for_ [1 .. 10] $ \\i -> do
--         when (i >= 5) $
--           returnEarly e ("Returned early with " ++ show i)
--       pure "End of loop"
-- "Returned early with 5"
-- @
returnEarly ::
  (er :> es) =>
  EarlyReturn r er ->
  -- | Return early to the handler, with this value.
  r ->
  Eff es a
returnEarly = throw

-- |
-- @
-- >>> runPureEff $ evalState 10 $ \\st -> do
--       n <- get st
--       pure (2 * n)
-- 20
-- @
evalState ::
  -- | Initial state
  s ->
  -- | Stateful computation
  (forall st. State s st -> Eff (st :& es) a) ->
  -- | Result
  Eff es a
evalState s f = fmap fst (runState s f)

-- |
-- @
-- >>> runPureEff $ withState 10 $ \\st -> do
--       n <- get st
--       pure (\s -> (2 * n, s))
-- (20,10)
-- @
withState ::
  -- | Initial state
  s ->
  -- | Stateful computation
  (forall st. State s st -> Eff (st :& es) (s -> a)) ->
  -- | Result
  Eff es a
withState s f = do
  (g, s') <- runState s f
  pure (g s')

data Compound e1 e2 ss where
  Compound ::
    Proxy# s1 ->
    Proxy# s2 ->
    e1 s1 ->
    e2 s2 ->
    Compound e1 e2 (s1 :& s2)

compound ::
  h1 e1 ->
  -- | ͘
  h2 e2 ->
  Compound h1 h2 (e1 :& e2)
compound = Compound proxy# proxy#

inComp :: forall a b c r. (a :> b) => (b :> c) => ((a :> c) => r) -> r
inComp k = case have (cmp (has @a @b) (has @b @c)) of Dict -> k

withCompound ::
  forall h1 h2 e es r.
  (e :> es) =>
  Compound h1 h2 e ->
  -- | ͘
  (forall e1 e2. (e1 :> es, e2 :> es) => h1 e1 -> h2 e2 -> Eff es r) ->
  Eff es r
withCompound c f =
  case c of
    Compound (_ :: Proxy# st) (_ :: Proxy# st') h i ->
      inComp @st @e @es (inComp @st' @e @es (f h i))

withC1 ::
  forall e1 e2 ss es r.
  (ss :> es) =>
  Compound e1 e2 ss ->
  (forall st. (st :> es) => e1 st -> Eff es r) ->
  Eff es r
withC1 c f = withCompound c (\h _ -> f h)

withC2 ::
  forall e1 e2 ss es r.
  (ss :> es) =>
  Compound e1 e2 ss ->
  (forall st. (st :> es) => e2 st -> Eff es r) ->
  Eff es r
withC2 c f = withCompound c (\_ i -> f i)

putC :: forall ss es e. (ss :> es) => Compound e (State Int) ss -> Int -> Eff es ()
putC c i = withC2 c (\h -> put h i)

getC :: forall ss es e. (ss :> es) => Compound e (State Int) ss -> Eff es Int
getC c = withC2 c (\h -> get h)

-- TODO: Make this (s1 :> es, s2 :> es), like withC
runCompound ::
  e1 s1 ->
  -- | ͘
  e2 s2 ->
  (forall es'. Compound e1 e2 es' -> Eff (es' :& es) r) ->
  Eff (s1 :& (s2 :& es)) r
runCompound e1 e2 k = assoc1Eff (k (compound e1 e2))

-- |
-- @
-- >>> runPureEff $ yieldToList $ \\y -> do
--       yield y 1
--       yield y 2
--       yield y 100
-- ([1,2,100], ())
-- @
yieldToList ::
  (forall e1. Stream a e1 -> Eff (e1 :& es) r) ->
  -- | Yielded elements and final result
  Eff es ([a], r)
yieldToList f = do
  (as, r) <- yieldToReverseList f
  pure (reverse as, r)

-- | This is more efficient than 'yieldToList' because it gathers the
-- elements into a stack in reverse order. @yieldToList@ then reverses
-- that stack.
--
-- @
-- >>> runPureEff $ yieldToReverseList $ \\y -> do
--       yield y 1
--       yield y 2
--       yield y 100
-- ([100,2,1], ())
-- @
yieldToReverseList ::
  (forall e. Stream a e -> Eff (e :& es) r) ->
  -- | Yielded elements in reverse order, and final result
  Eff es ([a], r)
yieldToReverseList f = do
  evalState [] $ \(s :: State lo st) -> do
    r <- forEach (insertSecond . f) $ \i ->
      modify s (i :)
    as <- get s
    pure (as, r)

mapStream ::
  (e2 :> es) =>
  -- | Apply this function to all elements of the input stream.
  (a -> b) ->
  -- | Input stream
  (forall e1. Stream a e1 -> Eff (e1 :& es) r) ->
  Stream b e2 ->
  Eff es r
mapStream f = mapMaybe (Just . f)

mapMaybe ::
  (e2 :> es) =>
  -- | Yield from the output stream all of the elemnts of the input
  -- stream for which this function returns @Just@
  (a -> Maybe b) ->
  -- | Input stream
  (forall e1. Stream a e1 -> Eff (e1 :& es) r) ->
  Stream b e2 ->
  Eff es r
mapMaybe f s y = forEach s $ \a -> do
  case f a of
    Nothing -> pure ()
    Just b_ -> yield y b_

-- | Remove 'Nothing' elements from a stream.
catMaybes ::
  (e2 :> es) =>
  -- | Input stream
  (forall e1. Stream (Maybe a) e1 -> Eff (e1 :& es) r) ->
  Stream a e2 ->
  Eff es r
catMaybes s y = mapMaybe id s y

type Jump = EarlyReturn ()

withJump ::
  (forall j. Jump j -> Eff (j :& es) ()) ->
  -- | ͘
  Eff es ()
withJump = withEarlyReturn

jumpTo ::
  (j :> es) =>
  Jump j ->
  -- | ͘
  Eff es a
jumpTo tag = throw tag ()

unwrap :: (j :> es) => Jump j -> Maybe a -> Eff es a
unwrap j = \case
  Nothing -> jumpTo j
  Just a -> pure a

-- | Handle that allows you to run 'IO' operations
data IOE (e :: Effects) = MkIOE

-- | Run an 'IO' operation in 'Eff'
--
-- @
-- >>> runEff $ \\io -> do
--       effIO io (putStrLn "Hello world!")
-- Hello, world!
-- @
effIO ::
  (e :> es) =>
  IOE e ->
  IO a ->
  -- | ͘
  Eff es a
effIO MkIOE = UnsafeMkEff

-- | Run an 'Eff' whose only unhandled effect is 'IO'.
--
-- @
-- >>> runEff $ \\io -> do
--       effIO io (putStrLn "Hello world!")
-- Hello, world!
-- @
runEff ::
  (forall e es. IOE e -> Eff (e :& es) a) ->
  -- | ͘
  IO a
runEff eff = unsafeUnEff (eff MkIOE)

connect ::
  (forall e1. Coroutine a b e1 -> Eff (e1 :& es) r1) ->
  (forall e2. a -> Coroutine b a e2 -> Eff (e2 :& es) r2) ->
  forall e1 e2.
  (e1 :> es, e2 :> es) =>
  Eff
    es
    ( Either
        (r1, a -> Coroutine b a e2 -> Eff es r2)
        (r2, b -> Coroutine a b e1 -> Eff es r1)
    )
connect _ _ = error "connect unimplemented, sorry"

head' ::
  forall a b r es.
  (forall e. Coroutine a b e -> Eff (e :& es) r) ->
  forall e.
  (e :> es) =>
  Eff
    es
    ( Either
        r
        (a, b -> Coroutine a b e -> Eff es r)
    )
head' c = do
  r <- connect c (\a _ -> pure a) @_ @es
  pure $ case r of
    Right r' -> Right r'
    Left (l, _) -> Left l

newtype Writer w e = Writer (Stream w e)

runWriter ::
  (Monoid w) =>
  -- | ͘
  (forall e. Writer w e -> Eff (e :& es) r) ->
  Eff es (r, w)
runWriter f = runState mempty $ \st -> do
  forEach (insertSecond . f . Writer) $ \ww -> do
    modify st (<> ww)

execWriter ::
  (Monoid w) =>
  -- | ͘
  (forall e. Writer w e -> Eff (e :& es) r) ->
  Eff es w
execWriter f = fmap snd (runWriter f)

tell ::
  (e :> es) =>
  Writer w e ->
  -- | ͘
  w ->
  Eff es ()
tell (Writer y) = yield y
