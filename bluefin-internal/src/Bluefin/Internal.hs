{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_HADDOCK not-home #-}

module Bluefin.Internal where

import qualified Bluefin.Internal.Exception.Scoped as ScopedException
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception
import Control.Monad (forever)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Trans.Control (MonadBaseControl, StM, liftBaseWith, restoreM)
import qualified Control.Monad.Trans.Reader as Reader
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Type.Coercion (Coercion (Coercion))
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

type role Eff nominal representational

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

-- | Deprecated.  Use 'withEffToIO_' instead.
withEffToIO ::
  (e2 :> es) =>
  -- | Continuation with the unlifting function in scope.
  ((forall r. (forall e1. IOE e1 -> Eff (e1 :& es) r) -> IO r) -> IO a) ->
  IOE e2 ->
  Eff es a
withEffToIO k io = effIO io (k (\f -> unsafeUnEff (f io)))

withEffToIO' ::
  (e2 :> es) =>
  -- | Continuation with the unlifting function in scope.
  IOE e2 ->
  ((forall r. (forall e1. IOE e1 -> Eff (e1 :& es) r) -> IO r) -> IO a) ->
  Eff es a
withEffToIO' io k = withEffToIO k io

-- | This is equivalent to the 'withRunInIO' method of
-- 'MonadUnliftIO', but written in Bluefin-style.
withEffToIO_ ::
  (e :> es) =>
  IOE e ->
  -- | Continuation with the unlifting function in scope.
  ((forall r. Eff es r -> IO r) -> IO a) ->
  Eff es a
withEffToIO_ io k =
  withEffToIO (\effToIO -> k (\eff -> effToIO (\_ -> useImpl eff))) io

-- We can do the old API in terms of withEffToIO_
withEffToIO_' ::
  (e2 :> es) =>
  IOE e2 ->
  -- | Continuation with the unlifting function in scope.
  ((forall r. (forall e1. IOE e1 -> Eff (e1 :& es) r) -> IO r) -> IO a) ->
  Eff es a
withEffToIO_' io k =
  withEffToIO_ io (\effToIO -> k (\eff -> effToIO (weakenEff (subsume2 has) (eff io))))

-- We don't try to do anything sophisticated here.  I haven't thought
-- through all the consequences.

-- | You probably want to use 'withEffToIO_' instead.
instance (e :> es) => MonadUnliftIO (EffReader (IOE e) es) where
  withRunInIO ::
    ((forall a. EffReader (IOE e) es a -> IO a) -> IO b) ->
    EffReader (IOE e) es b
  withRunInIO k =
    MkEffReader $ \io -> do
      withEffToIO_ io $ \effToIO -> do
        k $ \(MkEffReader f) -> do
          effToIO (f io)

race ::
  (e2 :> es) =>
  (forall e. IOE e -> Eff (e :& es) a) ->
  (forall e. IOE e -> Eff (e :& es) a) ->
  IOE e2 ->
  Eff es a
race x y io = do
  r <- withEffToIO' io $ \toIO ->
    Async.race (toIO x) (toIO y)

  pure $ case r of
    Left a -> a
    Right a -> a

-- | Connect two coroutines.  Their execution is interleaved by
-- exchanging @a@s and @b@s. When the first yields its first @a@ it
-- starts the second (which is awaiting an @a@).
connectCoroutines ::
  forall es a b r.
  (forall e. Coroutine a b e -> Eff (e :& es) r) ->
  (forall e. a -> Coroutine b a e -> Eff (e :& es) r) ->
  -- | ͘
  Eff es r
connectCoroutines m1 m2 = unsafeProvideIO $ \io -> do
  av <- effIO io newEmptyMVar
  bv <- effIO io newEmptyMVar

  let t1 :: forall e. IOE e -> Eff (e :& es) r
      t1 io' = forEach (useImplUnder . m1) $ \a -> effIO io' $ do
        putMVar av a
        takeMVar bv

  let t2 :: forall e. IOE e -> Eff (e :& es) r
      t2 io' = do
        ainit <- effIO io' (takeMVar av)
        forEach (useImplUnder . m2 ainit) $ \b_ -> effIO io' $ do
          putMVar bv b_
          takeMVar av

  race (useImplUnder . t1) (useImplUnder . t2) io

-- | Old name for 'consumeStream'.  @receiveStream@ will be deprecated
-- in a future version.
receiveStream ::
  (forall e. Consume a e -> Eff (e :& es) r) ->
  (forall e. Stream a e -> Eff (e :& es) r) ->
  Eff es r
receiveStream = consumeStream

consumeStream ::
  -- | Each 'await' from the @Consume@ ...
  (forall e. Consume a e -> Eff (e :& es) r) ->
  -- | ... receives the value 'yield'ed from the @Stream@
  (forall e. Stream a e -> Eff (e :& es) r) ->
  Eff es r
consumeStream r s = connectCoroutines r (\() -> s)

-- | Argument-flipped version of 'consumeStream'
streamConsume ::
  forall a (es :: Effects) r.
  (forall (e :: Effects). Stream a e -> Eff (e :& es) r) ->
  (forall (e :: Effects). Consume a e -> Eff (e :& es) r) ->
  -- | ͘
  Eff es r
streamConsume s c = consumeStream c s

zipCoroutines ::
  (e1 :> es) =>
  Coroutine (a1, a2) b e1 ->
  (forall e. Coroutine a1 b e -> Eff (e :& es) r) ->
  (forall e. Coroutine a2 b e -> Eff (e :& es) r) ->
  -- | ͘
  Eff es r
zipCoroutines c m1 m2 = do
  connectCoroutines m1 $ \a1 c1 -> do
    connectCoroutines (useImplUnder . m2) $ \a2 c2 -> do
      evalState (a1, a2) $ \ass -> do
        forever $ do
          as <- get ass
          b' <- yieldCoroutine c as
          a1' <- yieldCoroutine c1 b'
          a2' <- yieldCoroutine c2 b'
          put ass (a1', a2')

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
-- >>> runEff_ $ \\io -> withMonadIO io $ liftIO $ do
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

-- | Run an 'Eff' that doesn't contain any unhandled effects.
runPureEff :: (forall es. Eff es a) -> a
runPureEff e = unsafePerformIO (runEff_ (\_ -> e))

unsafeCoerceEff :: Eff t r -> Eff t' r
unsafeCoerceEff = coerce

weakenEff :: t `In` t' -> Eff t r -> Eff t' r
weakenEff _ = unsafeCoerceEff

insertFirst :: Eff b r -> Eff (c1 :& b) r
insertFirst = weakenEff (drop (eq (# #)))

insertSecond :: Eff (c1 :& b) r -> Eff (c1 :& (c2 :& b)) r
insertSecond = insertManySecond

insertManySecond :: (b :> c) => Eff (c1 :& b) r -> Eff (c1 :& c) r
insertManySecond = weakenEff (bimap has has)

assoc1Eff :: Eff ((a :& b) :& c) r -> Eff (a :& (b :& c)) r
assoc1Eff = weakenEff (assoc1 (# #))

pushFirst :: Eff a r -> Eff (a :& b) r
pushFirst = weakenEff (fstI (# #))

mergeEff :: Eff (a :& a) r -> Eff a r
mergeEff = weakenEff (merge (# #))

inContext :: (e2 :> e1) => Eff (e1 :& e2) r -> Eff e1 r
inContext = weakenEff (subsume1 has)

-- | Used to define dynamic effects.
makeOp :: Eff (e :& e) r -> Eff e r
makeOp = inContext

-- | Used to define dynamic effects.
useImpl :: (e :> es) => Eff e r -> Eff es r
useImpl = weakenEff has

-- | Like 'useImpl'
useImplUnder ::
  (e :> es) =>
  Eff (e1 :& e) r ->
  -- | ͘
  Eff (e1 :& es) r
useImplUnder = insertManySecond

-- | Used to define handlers of compound effects.
useImplIn ::
  (e :> es) =>
  (t -> Eff (es :& e) r) ->
  t ->
  -- | ͘
  Eff es r
useImplIn f h = inContext (f h)

-- | Deprecated.  Use 'useImplUnder' instead.
useImplWithin ::
  (e :> es) =>
  (t -> Eff (e1 :& e) r) ->
  t ->
  -- | ͘
  Eff (e1 :& es) r
useImplWithin k = useImplUnder . k

-- | Handle to a capability to create strict mutable state handles
data StateSource (e :: Effects) = StateSource

type role StateSource nominal

-- | Handle to an exception of type @exn@
newtype Exception exn (e :: Effects)
  = MkException (forall a. exn -> Eff e a)

-- | A handle to a strict mutable state of type @s@
newtype State s (e :: Effects) = UnsafeMkState (IORef s)

type role State representational nominal

-- | A handle to a coroutine that yields values of type @a@ and then
-- expects values of type @b@.
newtype Coroutine a b (e :: Effects) = MkCoroutine (a -> Eff e b)

-- | A handle to a stream that yields values of type @a@.  It is
-- implemented as a handle to a coroutine that yields values of type
-- @a@ and then expects values of type @()@.
type Stream a = Coroutine a ()

type Consume a = Coroutine () a

-- | Every Bluefin handle should have an instance of class @Handle@.
-- Built-in handles, such as 'Exception', 'State' and 'IOE', come with
-- @Handle@ instances.
--
-- You should define a @Handle@ instance for each handle that you
-- define yourself.  As
-- an example, an "application" handle with a dynamic effect for
-- database queries, a concrete effect for application state and a
-- concrete effect for a logging effect might look like this:
--
-- @
-- data Application e = MkApplication
--   { queryDatabase :: forall e'. String -> Int -> Eff (e' :& e) [String],
--     applicationState :: State (Int, Bool) e,
--     logger :: Stream String e
--   }
-- @
--
-- To define @mapHandle@ for @Application@ you should apply
-- @mapHandle@ to all the fields that are themeselves handles and
-- apply 'useImplUnder' to all the fields that are dynamic effects:
--
-- @
-- instance Handle Application where
--   mapHandle
--     MkApplication
--       { queryDatabase = q,
--         applicationState = a,
--         logger = l
--       } =
--       MkApplication
--         { queryDatabase = \\s i -> useImplUnder (q s i),
--           applicationState = mapHandle a,
--           logger = mapHandle l
--         }
-- @
class Handle (h :: Effects -> Type) where
  -- | Used to create compound effects, i.e. handles that contain
  -- other handles.
  mapHandle :: (e :> es) => h e -> h es

instance Handle (State s) where
  mapHandle (UnsafeMkState s) = UnsafeMkState s

instance Handle (Exception s) where
  mapHandle (MkException s) = MkException (weakenEff has . s)

instance Handle (Coroutine a b) where
  mapHandle (MkCoroutine f) = MkCoroutine (fmap useImpl f)

instance Handle (Writer w) where
  mapHandle (Writer wr) = Writer (mapHandle wr)

instance Handle IOE where
  mapHandle MkIOE = MkIOE

newtype In (a :: Effects) (b :: Effects) = In# (# #)

merge :: (# #) -> (a :& a) `In` a
merge (# #) = In# (# #)

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

subsume1 :: (e2 `In` e1) -> (e1 :& e2) `In` e1
subsume1 i = cmp (bimap (eq (# #)) i) (merge (# #))

subsume2 :: (e1 `In` e2) -> (e1 :& e2) `In` e2
subsume2 i = cmp (bimap i (eq (# #))) (merge (# #))

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
  (e :> es) =>
  Exception ex e ->
  -- | Value to throw
  ex ->
  Eff es a
throw h = case mapHandle h of MkException throw_ -> throw_

has :: forall a b. (a :> b) => a `In` b
has = In# (# #)

data Dict c where
  Dict :: forall c. (c) => Dict c

unsafeCoerceDict :: forall c c'. Dict c -> Dict c'
unsafeCoerceDict = unsafeCoerce @(Dict c) @(Dict c')

-- Seems like it could be better
have :: forall a b. a `In` b -> Dict (a :> b)
have _ = unsafeCoerceDict @(a :> (a :& b)) @(a :> b) Dict

-- |
-- @
-- >>> runPureEff $ try $ \\e -> do
--       throw e 42
--       pure "No exception thrown"
-- Left 42
-- @
try ::
  forall exn (es :: Effects) a.
  (forall e. Exception exn e -> Eff (e :& es) a) ->
  -- | @Left@ if the exception was thrown, @Right@ otherwise
  Eff es (Either exn a)
try f =
  unsafeProvideIO $ \io -> do
    withEffToIO_ io $ \effToIO -> do
      withScopedException_ $ \throw_ -> do
        effToIO (f (MkException (effIO io . throw_)))

-- | 'handle', but with the argument order swapped
--
-- @
-- >>> runPureEff $ handle (pure . show) $ \\e -> do
--       throw e 42
--       pure "No exception thrown"
-- "42"
-- @
handle ::
  forall exn (es :: Effects) a.
  -- | If the exception is thrown, apply this handler
  (exn -> Eff es a) ->
  (forall e. Exception exn e -> Eff (e :& es) a) ->
  Eff es a
handle h f =
  try f >>= \case
    Left e -> h e
    Right a -> pure a

catch ::
  forall exn (es :: Effects) a.
  (forall e. Exception exn e -> Eff (e :& es) a) ->
  -- | If the exception is thrown, apply this handler
  (exn -> Eff es a) ->
  Eff es a
catch f h = handle h f

-- We really do need to have an IOE argument here because, it is not
-- determined which of Ex1 or Ex2 is produced in the following code,
-- due to pure exceptions being imprecise.
--
-- f :: IO (Either Ex1 (Either Ex2 a))
-- f = runEff_ $ \io -> do
--   try $ \e1 -> do
--     try $ \e2 -> do
--       rethrowIO io e1 $ do
--         rethrowIO io e2 $ doa
--           Control.Exception.throw Ex1) (Control.Exception.throw E2)

-- | Rethrow an exception raised by an 'IO' action as a Bluefin
-- exception.
--
-- @
-- 'runEff' $ \\io -> do
--   r \<- 'try' $ \\ex -> do
--     rethrowIO @'Control.Exception.IOException' io ex $ do
--       effIO io ('Prelude.readFile' "\/tmp\/doesnt-exist")
--
--   'effIO' io $ putStrLn $ case r of
--     Left e -> "Caught IOException:\\n" ++ show e
--     Right contents -> contents
-- @
--
-- @
-- Caught IOException:
-- \/tmp\/doesnt-exist: openFile: does not exist (No such file or directory)
-- @
rethrowIO ::
  forall ex es e1 e2 r.
  (e1 :> es, e2 :> es, Control.Exception.Exception ex) =>
  IOE e1 ->
  Exception ex e2 ->
  Eff es r ->
  -- | ͘
  Eff es r
rethrowIO io ex body =
  withEffToIO_ io $ \effToIO -> do
    Control.Exception.catch
      (effToIO body)
      (\e -> effToIO (throw @_ @es ex e))

-- | @bracket acquire release body@: @acquire@ a resource, perform the
-- @body@ with it, and @release@ the resource even if @body@ threw an
-- exception.  This is essentially the same as
-- @Control.Exception.'Control.Exception.bracket'@, whose
-- documentation you can inspect for further details.
--
-- @bracket@ has a very general type that does not require @es@ to
-- contain an exception or IO effect. The reason that this is safe is:
--
--    * While @bracket@ does catch exceptions, this is unobservable,
--      since the exception is re-thrown; the cleanup action happens
--      unconditionally; and no part of it gets access to the thrown
--      exception.
--
--    * 'Eff' itself is able to guarantee that any exceptions thrown
--      in the body will be actually thrown before @bracket@
--      exits. This is inherited from the fact that @Eff@ is a wrapper
--      around 'IO'.
--
-- While it is usually the case that the cleanup action will in fact
-- want to use @IO@ effects, this is not universally true, see the
-- @polymorphicBracket@ example for an example.
bracket ::
  -- | Acquire the resource
  Eff es a ->
  -- | Release the resource
  (a -> Eff es ()) ->
  -- | Run the body
  (a -> Eff es b) ->
  Eff es b
bracket before after body =
  unsafeProvideIO $ \io -> do
    withEffToIO_ io $ \effToIO -> do
      Control.Exception.bracket
        (effToIO (useImpl before))
        (effToIO . useImpl . after)
        (effToIO . useImpl . body)

withStateInIO ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  State s e2 ->
  (IORef s -> IO r) ->
  Eff es r
withStateInIO io (UnsafeMkState r) k = effIO io (k r)

-- |
-- @
-- >>> runPureEff $ runState 10 $ \\st -> do
--       n <- get st
--       pure (2 * n)
-- (20,10)
-- @
get ::
  (e :> es) =>
  State s e ->
  -- | The current value of the state
  Eff es s
get st = unsafeProvideIO $ \io -> withStateInIO io st readIORef

-- | Set the value of the state
--
-- @
-- >>> runPureEff $ runState 10 $ \\st -> do
--       put st 30
-- ((), 30)
-- @
put ::
  (e :> es) =>
  State s e ->
  -- | The new value of the state.  The new value is forced before
  -- writing it to the state.
  s ->
  Eff es ()
put st s = unsafeProvideIO $ \io -> withStateInIO io st (flip writeIORef s)

-- |
-- @
-- >>> runPureEff $ runState 10 $ \\st -> do
--       modify st (* 2)
-- ((), 20)
-- @
modify ::
  (e :> es) =>
  State s e ->
  -- | Apply this function to the state.  The new value of the state
  -- is forced before writing it to the state.
  (s -> s) ->
  Eff es ()
modify state f = do
  s <- get state
  put state (f s)

withScopedException_ :: ((forall a. e -> IO a) -> IO r) -> IO (Either e r)
withScopedException_ f =
  ScopedException.try (\ex -> f (ScopedException.throw ex))

-- |
-- @
-- 'runPureEff' $ 'withStateSource' $ \\source -> do
--   n <- 'newState' source 5
--   total <- newState source 0
--
--   'withJump' $ \\done -> forever $ do
--     n' <- 'Bluefin.State.get' n
--     'Bluefin.State.modify' total (+ n')
--     when (n' == 0) $ 'Bluefin.Jump.jumpTo' done
--     modify n (subtract 1)
--
--   get total
-- 15
-- @
withStateSource ::
  (forall e. StateSource e -> Eff (e :& es) a) ->
  -- | ͘
  Eff es a
withStateSource f = useImplIn f StateSource

-- |
-- @
-- runPureEff $ 'withStateSource' $ \\source -> do
--   n <- 'newState' source 5
--   total <- newState source 0
--
--   'Bluefin.Jump.withJump' $ \\done -> forever $ do
--     n' <- 'Bluefin.State.get' n
--     'Bluefin.State.modify' total (+ n')
--     when (n' == 0) $ 'Bluefin.Jump.jumpTo' done
--     modify n (subtract 1)
--
--   get total
-- 15
-- @
newState ::
  (e :> es) =>
  StateSource e ->
  -- | The initial value for the state handle
  s ->
  -- | A new state handle
  Eff es (State s e)
newState StateSource s = unsafeProvideIO $ \io -> do
  fmap UnsafeMkState (effIO io (newIORef s))

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
  (forall e. State s e -> Eff (e :& es) a) ->
  -- | Result and final state
  Eff es (a, s)
runState s f = do
  withStateSource $ \source -> do
    state <- newState source s
    a <- f state
    s' <- get state
    pure (a, s')

-- |
-- @
-- >>> runPureEff $ runManyStates [10,15,20] $ mapM
--       (\st -> do
--          n <- get st
--          pure (2 * n)
--       )
-- ([20,30,40],[10,15,20])
-- @
runManyStates
  :: Traversable t
  => t s
  -> (forall (e :: Effects). t (State s e) -> Eff (e :& es) a)
  -> Eff es (a, t s)
runManyStates ss f =
  withStateSource $ \source -> do
    states <- traverse (newState source) ss
    a <- f states
    ss' <- traverse get states
    pure (a, ss')

yieldCoroutine ::
  (e1 :> es) =>
  Coroutine a b e1 ->
  -- | ͘
  a ->
  Eff es b
yieldCoroutine (MkCoroutine f) = useImpl . f

-- |
--
-- Yield an element to the stream.
--
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
--
-- Apply an effectful function to each element yielded to the stream.
--
-- @
-- >>> runPureEff $ yieldToList $ \\y -> do
--       for_ [0 .. 4] $ \\i -> do
--         yield y i
--         yield y (i * 10)
-- ([0, 0, 1, 10, 2, 20, 3, 30], ())
-- @
forEach ::
  (forall e1. Coroutine a b e1 -> Eff (e1 :& es) r) ->
  -- | Apply this effectful function for each element of the coroutine
  (a -> Eff es b) ->
  Eff es r
forEach f h = useImplIn f (MkCoroutine h)

-- |
--
-- Ignore all elements yielded to the stream.
--
-- @
-- >>> runPureEff $ ignoreStream $ \\y -> do
--      for_ [0 .. 4] $ \\i -> do
--        yield y i
--        yield y (i * 10)
--
--      pure 42
-- 42
-- @
ignoreStream ::
  (forall e1. Stream a e1 -> Eff (e1 :& es) r) ->
  -- | ͘
  Eff es r
ignoreStream k = forEach k (\_ -> pure ())

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
  evalState n $ \i -> forEach (useImplUnder . ss) $ \s -> do
    ii <- get i
    yield st (ii, s)
    put i (ii + 1)

-- | A version of 'forEach' specialized to @Consume@.  Every time the
-- @Consume@ is used to 'await' a @b@, feed it the one created by the
-- handler.
consumeEach ::
  -- | Body
  ( forall e.
    Consume b e ->
    Eff (e :& es) r
  ) ->
  -- | Value to send to each @await@ in the body.
  Eff es b ->
  Eff es r
consumeEach k e = forEach k (\() -> e)

await :: (e :> es) => Consume a e -> Eff es a
await r = yieldCoroutine r ()

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
  (forall e. EarlyReturn r e -> Eff (e :& es) r) ->
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
  (e :> es) =>
  EarlyReturn r e ->
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
  (forall e. State s e -> Eff (e :& es) a) ->
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
  (forall e. State s e -> Eff (e :& es) (s -> a)) ->
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
--
-- Gather all yielded elements into a list.
--
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

-- |
-- @
-- >>> runPureEff $ withYieldToList $ \\y -> do
--   yield y 1
--   yield y 2
--   yield y 100
--   pure length
-- 3
-- @
withYieldToList ::
  -- | Stream computation
  (forall e. Stream a e -> Eff (e :& es) ([a] -> r)) ->
  -- | Result
  Eff es r
withYieldToList f = do
  (l, g) <- yieldToList f
  pure (g l)

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
    r <- forEach (useImplUnder . f) $ \i ->
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

-- |
-- @
-- runPureEff $ yieldToList $ \yOut -> do
--   consumeStream
--     (\c -> takeConsume 6 c yOut)
--     (\yIn -> cycleToStream [1..3] yIn)
-- ([1,2,3,1,2,3],())
-- @
cycleToStream ::
  (Foldable f, ea :> es) =>
  f a ->
  Stream a ea ->
  -- | ͘
  Eff es ()
cycleToStream f y = do
  forever (inFoldable f y)

-- |
-- @
-- runPureEff $ yieldToList $ \yOut -> do
--   consumeStream
--     (\c -> takeConsume 4 c yOut)
--     (\yIn -> inFoldable [1..10] yIn)
-- ([1,2,3,4],())
-- @
takeConsume ::
  (ea :> es, eb :> es) =>
  Int ->
  Consume a ea ->
  Stream a eb ->
  -- | ͘
  Eff es ()
takeConsume count source sink = loop count
  where
    loop c | c <= 0 = pure ()
    loop c = do
      await source >>= yield sink
      loop (c - 1)

type Jump = EarlyReturn ()

-- |
-- @
-- runPureEff $ 'withStateSource' $ \\source -> do
--   n <- 'newState' source 5
--   total <- newState source 0
--
--   'Bluefin.Jump.withJump' $ \\done -> forever $ do
--     n' <- 'Bluefin.State.get' n
--     'Bluefin.State.modify' total (+ n')
--     when (n' == 0) $ 'Bluefin.Jump.jumpTo' done
--     modify n (subtract 1)
--
--   get total
-- 15
-- @
withJump ::
  (forall e. Jump e -> Eff (e :& es) ()) ->
  -- | ͘
  Eff es ()
withJump = withEarlyReturn

-- |
-- @
-- runPureEff $ 'withStateSource' $ \\source -> do
--   n <- 'newState' source 5
--   total <- newState source 0
--
--   'Bluefin.Jump.withJump' $ \\done -> forever $ do
--     n' <- 'Bluefin.State.get' n
--     'Bluefin.State.modify' total (+ n')
--     when (n' == 0) $ 'Bluefin.Jump.jumpTo' done
--     modify n (subtract 1)
--
--   get total
-- 15
-- @
jumpTo ::
  (e :> es) =>
  Jump e ->
  -- | ͘
  Eff es a
jumpTo tag = throw tag ()

unwrap :: (e :> es) => Jump e -> Maybe a -> Eff es a
unwrap j = \case
  Nothing -> jumpTo j
  Just a -> pure a

-- | Handle that allows you to run 'IO' operations
data IOE (e :: Effects) = MkIOE

type role IOE nominal

-- | Run an 'IO' operation in 'Eff'
--
-- @
-- >>> runEff_ $ \\io -> do
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
-- >>> runEff_ $ \\io -> do
--       effIO io (putStrLn "Hello world!")
-- Hello, world!
-- @
--
-- We suggest you use 'runEff_' instead, as it probably has better
-- type inference properties.
runEff ::
  (forall e es. IOE e -> Eff (e :& es) a) ->
  -- | ͘
  IO a
runEff eff = runEff_ (makeOp . eff)

-- | Run an 'Eff' whose only unhandled effect is 'IO'.
--
-- @
-- >>> runEff_ $ \\io -> do
--       effIO io (putStrLn "Hello world!")
-- Hello, world!
-- @
--
-- This probably has better type inference properties than 'runEff'
-- and so will probably replace it in a later version.
runEff_ ::
  (forall e. IOE e -> Eff e a) ->
  -- | ͘
  IO a
runEff_ eff = unsafeUnEff (eff MkIOE)

unsafeProvideIO ::
  (forall e. IOE e -> Eff (e :& es) a) ->
  -- | ͘
  Eff es a
unsafeProvideIO eff = useImplIn eff MkIOE

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

-- |
-- @
-- >>> 'Data.Monoid.getAny' $ snd $ runPureEff $ runWriter $ \\w -> do
--       -- Non-empty list (the tell event does happen)
--       for_ [1 .. 10] $ \\_ -> tell w ('Data.Monoid.Any' True)
-- True
-- @
runWriter ::
  (Monoid w) =>
  -- | ͘
  (forall e. Writer w e -> Eff (e :& es) r) ->
  Eff es (r, w)
runWriter f = runState mempty $ \st -> do
  forEach (useImplUnder . f . Writer) $ \ww -> do
    modify st (<> ww)

-- |
-- @
-- >>> 'Data.Monoid.getAny' $ runPureEff $ execWriter $ \\w -> do
--       -- Non-empty list (the tell event does happen)
--       for_ [1 .. 10] $ \\_ -> tell w ('Data.Monoid.Any' True)
-- True
-- @
--
-- @
-- >>> 'Data.Monoid.getAny' $ runPureEff $ execWriter $ \\w -> do
--       -- Empty list (the tell event does not happen)
--       for_ [] $ \\_ -> tell w ('Data.Monoid.Any' True)
-- False
-- @
execWriter ::
  (Monoid w) =>
  -- | ͘
  (forall e. Writer w e -> Eff (e :& es) r) ->
  Eff es w
execWriter f = fmap snd (runWriter f)

-- |
-- @
-- >>> 'Data.Monoid.getAny' $ runPureEff $ execWriter $ \\w -> do
--       -- Non-empty list (the tell event does happen)
--       for_ [1 .. 10] $ \\_ -> tell w ('Data.Monoid.Any' True)
-- True
-- @
tell ::
  (e :> es) =>
  Writer w e ->
  -- | ͘
  w ->
  Eff es ()
tell (Writer y) = yield y

newtype Reader r e = MkReader (State r e)
  deriving newtype (Handle)

runReader ::
  -- | Initial value for @Reader@.
  r ->
  (forall e. Reader r e -> Eff (e :& es) a) ->
  Eff es a
runReader r f = evalState r (f . MkReader)

-- | Read the value.  Note that @ask@ has the property that these two
-- operations are always equivalent:
--
-- @
-- do
--   r1 <- ask re
--   r2 <- ask re
--   pure (r1, r2)
-- @
--
-- @
-- do
--   r <- ask re
--   pure (r, r)
-- @
ask ::
  (e :> es) =>
  -- | ͘
  Reader r e ->
  Eff es r
ask (MkReader st) = get st

-- | Read the value modified by a function
asks ::
  (e :> es) =>
  Reader r e ->
  -- | Read the value modified by this function
  (r -> a) ->
  Eff es a
asks (MkReader st) f = fmap f (get st)

local ::
  (e1 :> es) =>
  Reader r e1 ->
  -- | In the body, the reader value is modified by this function.
  (r -> r) ->
  -- | Body
  Eff es a ->
  Eff es a
local (MkReader st) f k = do
  orig <- get st
  bracket
    (put st (f orig))
    (\() -> put st orig)
    (\() -> k)

newtype HandleReader h e = UnsafeMkHandleReader (State (h e) e)

-- In general this is really tremendously unsafe because we could take
-- an `HandleReader h e`, map it to `HandleReader h es`, write an `h
-- es` to it and then use the original handle to use the `h es` at
-- type `h e`.  We must be very careful to only ever write to the
-- mapped handle in such a way that we can only use the new value at
-- the correct effect tag.  That is, we should only ever write to the
-- handle via `localHandle`.
mapHandleReader ::
  forall h e es.
  (Handle h, e :> es) =>
  HandleReader h e ->
  -- | ͘
  HandleReader h es
mapHandleReader = case coerceH of Coercion -> coerce
  where
    coerceH :: Coercion (h e) (h es)
    coerceH = unsafeCoerce (Coercion :: Coercion (h e) (h e))

localHandle ::
  (e :> es, Handle h) =>
  HandleReader h e ->
  (h es -> h es) ->
  Eff es r ->
  -- | ͘
  Eff es r
localHandle hh@(UnsafeMkHandleReader st) f k = do
  let UnsafeMkHandleReader st' = mapHandle hh
  orig <- get st
  bracket
    (put st' (f (mapHandle orig)))
    (\() -> put st orig)
    (\() -> k)

askHandle ::
  (e :> es, Handle h) =>
  HandleReader h e ->
  -- | ͘
  Eff es (h es)
askHandle hh = let UnsafeMkHandleReader st = mapHandle hh in get st

runHandleReader ::
  (e1 :> es, Handle h) =>
  h e1 ->
  (forall e. HandleReader h e -> Eff (e :& es) r) ->
  -- | ͘
  Eff es r
runHandleReader h k = do
  evalState (mapHandle h) $ \(st :: State (h es) e) -> do
    let coerceH :: Coercion (h es) (h (e :& es))
        coerceH = unsafeCoerce (Coercion :: Coercion (h es) (h es))

    let mapS :: State (h es) e' -> State (h (e :& es)) e'
        mapS = case coerceH of Coercion -> coerce

    let h' :: HandleReader h (e :& es)
        h' = case coerceH of
          Coercion ->
            UnsafeMkHandleReader (mapS (mapHandle st))

    useImplIn k h'

instance (Handle h) => Handle (HandleReader h) where mapHandle = mapHandleReader

newtype ConstEffect r (e :: Effects) = MkConstEffect r

runConstEffect ::
  r ->
  (forall e. ConstEffect r e -> Eff (e :& es) a) ->
  -- | ͘
  Eff es a
runConstEffect r k = useImplIn k (MkConstEffect r)

instance Handle (ConstEffect r) where
  mapHandle = coerce
