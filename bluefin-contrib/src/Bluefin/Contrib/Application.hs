module Bluefin.Contrib.Application
  ( -- | An opinionated, generic framework of an application taking
    -- events and producing output with configurable input and output
    -- types and a separation between application logic and state
    -- storage component.
    TaggedInput,
    Application(..),
    RunApplication,
    IOApplicationConfig(..),

    runApplication,

    -- | Run an application with no side effects
    runApplicationPure,

    -- | Start an application with IO
    withApplicationIO,

    -- | Run application as a test simulation.
    runApplicationIO,
    -- | Run application as a test simulation and ignore exceptions.
    runApplicationIO'
  )
where

import Bluefin.Compound
import Bluefin.Coroutine
import Bluefin.Eff
import Bluefin.Exception
import Bluefin.IO
import Bluefin.State
import Bluefin.Stream
import Control.Concurrent hiding (yield)
import qualified Control.Exception
import Control.Monad
import Data.Default
import Data.Either
import Data.Fixed
import Data.Function
import Data.IORef
import Data.Time
import Data.Time.Clock.System

type TaggedInput a = Maybe (UTCTime, Maybe a)

type RunApplication s a i o = forall e1 es. e1 :> es => a -> (UTCTime, Maybe i) -> Application i o s e1 -> Eff es a

type RunStore s e = forall e1 e2 es r. (e1 :> es, e2 :> es) => IOE e1 -> Exception e e2 -> (forall v. s v -> Eff (v :& es) r) -> Eff es r

data Application i o s e = Application
  { -- | Chunked output to match each input
    emitter :: Coroutine [o] (TaggedInput i) e,
    store :: s e
  }

instance Handle s => Handle (Application i o s) where
  mapHandle Application
    { emitter = em
    , store = st
    } =
    Application
    { emitter = mapHandle em,
      store = mapHandle st
    }

runApplication
  :: (e1 :> es, e2 :> es, Handle s)
  => Coroutine [o] (TaggedInput i) e1
  -> s e2
  -> (forall e. Application i o s e -> Eff (e :& es) r)
  -> Eff es r
runApplication c s k = useImplIn k (Application (mapHandle c) (mapHandle s))

-- | Run an application with a pure environment
runApplicationPure
  :: Handle s
  => a
  -> b
  -> (forall e1 es r. e1 :> es => State b e1 -> (forall e. s e -> Eff (e :& es) r) -> Eff es r)
  -> RunApplication s a i o
  -> [(UTCTime, Maybe i)]
  -> ([[o]], (a, b))
runApplicationPure initialApp initialState runStore appLoop input = runPureEff $
  fmap (\((a,b),c) -> (a,(b,c))) $ runState initialState $ \s ->
  yieldToList $ \output ->
  runStore s $ \store ->
  connectCoroutines
  (\y -> do
      mapM_ (yieldCoroutine y . Just >=> yield output) input
      _ <- yieldCoroutine y Nothing
      undefined
  )
  (\a em -> maybe (pure initialApp) (\b -> runApplication em store (appLoop initialApp b)) a
  )

data IOApplicationConfig i o = IOApplicationConfig
  { pulseInterval :: NominalDiffTime,
    firstPulseDelay :: NominalDiffTime,
    testLength :: NominalDiffTime,
    testFakeStart :: Maybe UTCTime,
    reaction :: o -> IO (Maybe i)
  }

instance Default (IOApplicationConfig i o) where
  def = IOApplicationConfig
    { pulseInterval = 0.01,
      firstPulseDelay = 0,
      testLength = 0.1,
      testFakeStart = Just (UTCTime systemEpochDay 0),
      reaction = const (pure Nothing)
    }

timeFunc :: IOApplicationConfig i o -> IO (IO UTCTime)
timeFunc cfg
  | Just epoch <- testFakeStart cfg = getCurrentTime >>= \start -> pure $ do
      t <- getCurrentTime
      let d = diffUTCTime t start
      pure $ addUTCTime d epoch
  | otherwise = pure getCurrentTime

threadDelay' :: NominalDiffTime -> IO ()
threadDelay' t = threadDelay $ (nominalDiffTimeToSeconds t * 1000000) `div'` 1

runApplicationIO
  :: (Handle s, Control.Exception.Exception e)
  => a
  -> IOApplicationConfig i o
  -> RunStore s e
  -> RunApplication s a i o
  -> [(NominalDiffTime, Maybe i)]
  -> IO ([(UTCTime, (Either e o))], a)
runApplicationIO initialApp cfg runStore appLoop xs = do
  let life = testLength cfg
  getTestTime <- timeFunc cfg
  outputTimed <- newIORef []
  finalState <- withApplicationIO initialApp cfg runStore appLoop $ \pulse output input -> do
    let delayedEvent (t,ev) = do
          threadDelay' t
          maybe pulse (writeChan input) ev
    mapM_ (forkIO . delayedEvent) xs
    Control.Exception.bracket
      (forkIO $ forever $ do
          o <- readChan output
          (modifyIORef outputTimed . (:)) =<< (,o) <$> getTestTime
      )
      killThread
      (const $ threadDelay' life)

  (,finalState) . reverse <$> readIORef outputTimed

runApplicationIO'
  :: Handle s
  => a
  -> IOApplicationConfig i o
  -> RunStore s Control.Exception.SomeException
  -> RunApplication s a i o
  -> [(NominalDiffTime, Maybe i)]
  -> IO ([(UTCTime, o)], a)
runApplicationIO' initialApp cfg runStore appLoop =
  fmap (\(xs, a) -> (rights $ map sequence xs, a)) .
  runApplicationIO initialApp cfg runStore appLoop

withApplicationIO
  :: forall i a o s e b. (Handle s, Control.Exception.Exception e)
  => a
  -> IOApplicationConfig i o
  -> RunStore s e
  -> RunApplication s a i o
  -> (IO () -> Chan (Either e o) -> Chan i -> IO b)
  -> IO a
withApplicationIO initialApp cfg runStore appLoop f = Control.Exception.bracket
  (do
      output <- newChan
      reactionChan <- dupChan output
      input <- newChan
      getApplicationTime <- timeFunc cfg

      pulser <- forkIO $ do
        threadDelay' $ firstPulseDelay cfg
        forever $ do
          writeChan input $ Just Nothing
          threadDelay' $ pulseInterval cfg

      finalState <- newEmptyMVar
      worker <- forkIO $ runEff $ \io -> do
        result <- fix $ \mainLoop -> do
          res <- try $ \exception ->
            runStore io exception $ \store ->
            fmap snd $ runState initialApp $ \st ->
            connectCoroutines
            (\y -> forever $
                effIO io (do
                             inp <- readChan input
                             now <- getApplicationTime
                             pure $ (now,) <$> inp
                         ) >>=
                yieldCoroutine y >>= effIO io . mapM_ (writeChan output . Right)
            )
            (\a y -> do
                appState <- get st
                maybe (pure ())
                  (\b -> put st =<< runApplication y store (appLoop appState b)) a
            )
          case res of
            Right x -> pure x
            Left err -> do
              effIO io $ writeChan output $ Left err
              mainLoop
        effIO io $ do
          putMVar finalState result

      reactionThread <- forkIO $ forever $
        readChan reactionChan >>=
        either (const $ pure Nothing) (reaction cfg) >>=
        foldMap (writeChan input . Just . Just)

      wrappedInput <- newChan
      inputThread <- forkIO $ forever $
        writeChan input . Just . Just =<< readChan wrappedInput

      pure ( (output, input, wrappedInput, finalState),
             [pulser, worker, reactionThread, inputThread]
           )
  )
  (mapM_ killThread . snd)
  (\((output, input, wrappedInput, finalState), _) -> do
      _ <- f (writeChan input Nothing) output wrappedInput
      writeChan input Nothing
      takeMVar finalState
  )
