{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Bluefin.Internal.Examples where

import Bluefin.Internal hiding (w)
import Bluefin.Internal.Pipes
  ( Producer,
    runEffect,
    stdinLn,
    stdoutLn,
    takeWhile',
    (>->),
  )
import qualified Bluefin.Internal.Pipes as P
import Control.Exception (IOException)
import qualified Control.Exception
import Control.Monad (forever, replicateM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Foldable (for_, traverse_)
import Data.Monoid (Any (Any, getAny))
import Text.Read (readMaybe)
import Prelude hiding
  ( break,
    drop,
    head,
    read,
    readFile,
    return,
    writeFile,
  )
import qualified Prelude

monadIOExample :: IO ()
monadIOExample = runEff $ \io -> withMonadIO io $ liftIO $ do
  name <- readLn
  putStrLn ("Hello " ++ name)

monadFailExample :: Either String ()
monadFailExample = runPureEff $ try $ \e ->
  when ((2 :: Int) > 1) $
    withMonadFail e (fail "2 was bigger than 1")

throwExample :: Either Int String
throwExample = runPureEff $ try $ \e -> do
  _ <- throw e 42
  pure "No exception thrown"

handleExample :: String
handleExample = runPureEff $ handle (pure . show) $ \e -> do
  _ <- throw e (42 :: Int)
  pure "No exception thrown"

exampleGet :: (Int, Int)
exampleGet = runPureEff $ runState 10 $ \st -> do
  n <- get st
  pure (2 * n)

examplePut :: ((), Int)
examplePut = runPureEff $ runState 10 $ \st -> do
  put st 30

exampleModify :: ((), Int)
exampleModify = runPureEff $ runState 10 $ \st -> do
  modify st (* 2)

yieldExample :: ([Int], ())
yieldExample = runPureEff $ yieldToList $ \y -> do
  yield y 1
  yield y 2
  yield y 100

withYieldToListExample :: Int
withYieldToListExample = runPureEff $ withYieldToList $ \y -> do
  yield y 1
  yield y 2
  yield y 100
  pure length

-- This shows we can use forEach at any level of nesting with
-- insertManySecond
doubleNestedForEach ::
  (forall e. Stream () e -> Eff (e :& es) ()) ->
  Eff es ()
doubleNestedForEach f =
  withState () $ \_ -> do
    withState () $ \_ -> do
      forEach (insertManySecond . f) (\_ -> pure ())
      pure (\_ _ -> ())

forEachExample :: ([Int], ())
forEachExample = runPureEff $ yieldToList $ \y -> do
  forEach (inFoldable [0 .. 4]) $ \i -> do
    yield y i
    yield y (i * 10)

inFoldableExample :: ([Int], ())
inFoldableExample = runPureEff $ yieldToList $ inFoldable [1, 2, 100]

enumerateExample :: ([(Int, String)], ())
enumerateExample = runPureEff $ yieldToList $ enumerate (inFoldable ["A", "B", "C"])

returnEarlyExample :: String
returnEarlyExample = runPureEff $ withEarlyReturn $ \e -> do
  for_ [1 :: Int .. 10] $ \i -> do
    when (i >= 5) $
      returnEarly e ("Returned early with " ++ show i)
  pure "End of loop"

effIOExample :: IO ()
effIOExample = runEff $ \io -> do
  effIO io (putStrLn "Hello world!")

example1_ :: (Int, Int)
example1_ =
  let example1 :: Int -> Int
      example1 n = runPureEff $ evalState n $ \st -> do
        n' <- get st
        when (n' < 10) $
          put st (n' + 10)
        get st
   in (example1 5, example1 12)

example2_ :: ((Int, Int), (Int, Int))
example2_ =
  let example2 :: (Int, Int) -> (Int, Int)
      example2 (m, n) = runPureEff $
        evalState m $ \sm -> do
          evalState n $ \sn -> do
            do
              n' <- get sn
              m' <- get sm

              if n' < m'
                then put sn (n' + 10)
                else put sm (m' + 10)

            n' <- get sn
            m' <- get sm

            pure (n', m')
   in (example2 (5, 10), example2 (12, 5))

example3' :: Int -> Either String Int
example3' n = runPureEff $
  try $ \ex -> do
    evalState 0 $ \total -> do
      for_ [1 .. n] $ \i -> do
        soFar <- get total
        when (soFar > 20) $ do
          throw ex ("Became too big: " ++ show soFar)
        put total (soFar + i)

      get total

-- Count non-empty lines from stdin, and print a friendly message,
-- until we see "STOP".
example3_ :: IO ()
example3_ = runEff $ \io -> do
  let getLineUntilStop y = withJump $ \stop -> forever $ do
        line <- effIO io getLine
        when (line == "STOP") $
          jumpTo stop
        yield y line

      nonEmptyLines =
        mapMaybe
          ( \case
              "" -> Nothing
              line -> Just line
          )
          getLineUntilStop

      enumeratedLines = enumerateFrom 1 nonEmptyLines

      formattedLines =
        mapStream
          (\(i, line) -> show i ++ ". Hello! You said " ++ line)
          enumeratedLines

  forEach formattedLines $ \line -> effIO io (putStrLn line)

awaitList ::
  (e :> es) =>
  [a] ->
  IOE e ->
  (forall e1. Consume a e1 -> Eff (e1 :& es) ()) ->
  Eff es ()
awaitList l io k = evalState l $ \s -> do
  withJump $ \done ->
    bracket
      (pure ())
      (\() -> effIO io (putStrLn "Released"))
      $ \() -> do
        consumeEach (useImplWithin k) $ do
          (x, xs) <-
            get s >>= \case
              [] -> jumpTo done
              x : xs -> pure (x, xs)
          put s xs
          pure x

takeRec ::
  (e3 :> es) =>
  Int ->
  (forall e. Consume a e -> Eff (e :& es) ()) ->
  Consume a e3 ->
  Eff es ()
takeRec n k rec =
  withJump $ \done -> evalState n $ \s -> consumeEach (useImplWithin k) $ do
    s' <- get s
    if s' <= 0
      then jumpTo done
      else do
        modify s (subtract 1)
        await rec

mapRec ::
  (e :> es) =>
  (a -> b) ->
  (forall e1. Consume b e1 -> Eff (e1 :& es) ()) ->
  Consume a e ->
  Eff es ()
mapRec f = traverseRec (pure . f)

traverseRec ::
  (e :> es) =>
  (a -> Eff es b) ->
  (forall e1. Consume b e1 -> Eff (e1 :& es) ()) ->
  Consume a e ->
  Eff es ()
traverseRec f k rec = forEach k $ \() -> do
  r <- await rec
  f r

awaitUsage ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  (forall e. Consume () e -> Eff (e :& es) ()) ->
  Consume Int e2 ->
  Eff es ()
awaitUsage io x = do
  mapRec (* 11) $
    mapRec (subtract 1) $
      takeRec 3 $
        traverseRec (effIO io . print) $
          useImplWithin x

awaitExample :: IO ()
awaitExample = runEff $ \io -> do
  awaitList [1 :: Int ..] io $ awaitUsage io $ \rec -> do
    replicateM_ 5 (await rec)

consumeStreamExample :: IO (Either String String)
consumeStreamExample = runEff $ \io -> do
  try $ \ex -> do
    consumeStream
      ( \r ->
          bracket
            (effIO io (putStrLn "Starting 2"))
            (\_ -> effIO io (putStrLn "Leaving 2"))
            $ \_ -> do
              for_ [1 :: Int .. 100] $ \n -> do
                b' <- await r
                effIO
                  io
                  ( putStrLn
                      ("Consumed body " ++ show b' ++ " at time " ++ show n)
                  )
              pure "Consumer finished first"
      )
      ( \y -> bracket
          (effIO io (putStrLn "Starting 1"))
          (\_ -> effIO io (putStrLn "Leaving 1"))
          $ \_ -> do
            for_ [1 :: Int .. 10] $ \n -> do
              effIO io (putStrLn ("Sending " ++ show n))
              yield y n
              when (n > 5) $ do
                effIO io (putStrLn "Aborting...")
                throw ex "Aborted"

            pure "Yielder finished first"
      )

consumeStreamExample2 :: IO ()
consumeStreamExample2 = runEff $ \io -> do
  let counter yeven yodd = for_ [0 :: Int .. 10] $ \i -> do
        if even i
          then yield yeven i
          else yield yodd i

  let foo yeven =
        consumeStream
          ( \r -> forever $ do
              i <- await r
              effIO io (putStrLn ("Odd: " ++ show i))
          )
          (counter yeven)

  let bar =
        consumeStream
          ( \r -> forever $ do
              i <- await r
              effIO io (putStrLn ("Even: " ++ show i))
          )
          foo

  bar

connectExample :: IO (Either String String)
connectExample = runEff $ \io -> do
  try $ \ex -> do
    connectCoroutines
      ( \y -> bracket
          (effIO io (putStrLn "Starting 1"))
          (\_ -> effIO io (putStrLn "Leaving 1"))
          $ \_ -> do
            for_ [1 :: Int .. 10] $ \n -> do
              effIO io (putStrLn ("Sending " ++ show n))
              yield y n
              when (n > 5) $ do
                effIO io (putStrLn "Aborting...")
                throw ex "Aborted"

            pure "Yielder finished first"
      )
      ( \binit r ->
          bracket
            (effIO io (putStrLn "Starting 2"))
            (\_ -> effIO io (putStrLn "Leaving 2"))
            $ \_ -> do
              effIO io (putStrLn ("Consumed intial " ++ show binit))
              for_ [1 :: Int .. 100] $ \n -> do
                b' <- await r
                effIO
                  io
                  ( putStrLn
                      ("Consumed body " ++ show b' ++ " at time " ++ show n)
                  )
              pure "Consumer finished first"
      )

zipCoroutinesExample :: IO ()
zipCoroutinesExample = runEff $ \io -> do
  let m1 y = do
        r <- yieldCoroutine y 1
        evalState r $ \rs -> do
          for_ [1 .. 10 :: Int] $ \i -> do
            r' <- get rs
            r'' <- yieldCoroutine y (r' + i)
            put rs r''

  let m2 y = do
        r <- yieldCoroutine y 1
        evalState r $ \rs -> do
          for_ [1 .. 5 :: Int] $ \i -> do
            r' <- get rs
            r'' <- yieldCoroutine y (r' - i)
            put rs r''

  forEach (\c -> zipCoroutines c m1 m2) $ \i@(i1, i2) -> do
    effIO io (print i)
    pure (i1 + i2)

-- Count the number of (strictly) positives and (strictly) negatives
-- in a list, unless we see a zero, in which case we bail with an
-- error message.
countPositivesNegatives :: [Int] -> String
countPositivesNegatives is = runPureEff $
  evalState (0 :: Int) $ \positives -> do
    r <- try $ \ex ->
      evalState (0 :: Int) $ \negatives -> do
        for_ is $ \i -> do
          case compare i 0 of
            GT -> modify positives (+ 1)
            EQ -> throw ex ()
            LT -> modify negatives (+ 1)

        p <- get positives
        n <- get negatives

        pure $
          "Positives: "
            ++ show p
            ++ ", negatives "
            ++ show n

    case r of
      Right r' -> pure r'
      Left () -> do
        p <- get positives
        pure $
          "We saw a zero, but before that there were "
            ++ show p
            ++ " positives"

-- How to make compound effects

type MyHandle = Compound (State Int) (Exception String)

myInc :: (e :> es) => MyHandle e -> Eff es ()
myInc h = withCompound h (\s _ -> modify s (+ 1))

myBail :: (e :> es) => MyHandle e -> Eff es r
myBail h = withCompound h $ \s e -> do
  i <- get s
  throw e ("Current state was: " ++ show i)

runMyHandle ::
  (forall e. MyHandle e -> Eff (e :& es) a) ->
  Eff es (Either String (a, Int))
runMyHandle f =
  try $ \e -> do
    runState 0 $ \s -> do
      runCompound s e f

compoundExample :: Either String (a, Int)
compoundExample = runPureEff $ runMyHandle $ \h -> do
  myInc h
  myInc h
  myBail h

countExample :: IO ()
countExample = runEff $ \io -> do
  evalState @Int 0 $ \sn -> do
    withJump $ \break -> forever $ do
      n <- get sn
      when (n >= 10) (jumpTo break)
      effIO io (print n)
      modify sn (+ 1)

writerExample1 :: Bool
writerExample1 = getAny $ runPureEff $ execWriter $ \w -> do
  for_ [] $ \_ -> tell w (Any True)

writerExample2 :: Bool
writerExample2 = getAny $ runPureEff $ execWriter $ \w -> do
  for_ [1 .. 10] $ \_ -> tell w (Any True)

while :: Eff es Bool -> Eff es a -> Eff es ()
while condM body =
  withJump $ \break_ -> do
    forever $ do
      cond <- insertFirst condM
      unless cond (jumpTo break_)
      insertFirst body

stateSourceExample :: Int
stateSourceExample = runPureEff $ withStateSource $ \source -> do
  n <- newState source 5
  total <- newState source 0

  withJump $ \done -> forever $ do
    n' <- get n
    modify total (+ n')
    when (n' == 0) $ jumpTo done
    modify n (subtract 1)

  get total

incrementReadLine ::
  (e1 :> es, e2 :> es, e3 :> es) =>
  State Int e1 ->
  Exception String e2 ->
  IOE e3 ->
  Eff es ()
incrementReadLine state exception io = do
  withJump $ \break -> forever $ do
    line <- effIO io getLine
    i <- case readMaybe line of
      Nothing ->
        throw exception ("Couldn't read: " ++ line)
      Just i ->
        pure i

    when (i == 0) $
      jumpTo break

    modify state (+ i)

runIncrementReadLine :: IO (Either String Int)
runIncrementReadLine = runEff $ \io -> do
  try $ \exception -> do
    ((), r) <- runState 0 $ \state -> do
      incrementReadLine state exception io
    pure r

-- Counter 1

newtype Counter1 e = MkCounter1 (State Int e)

incCounter1 :: (e :> es) => Counter1 e -> Eff es ()
incCounter1 (MkCounter1 st) = modify st (+ 1)

runCounter1 ::
  (forall e. Counter1 e -> Eff (e :& es) r) ->
  Eff es Int
runCounter1 k =
  evalState 0 $ \st -> do
    _ <- k (MkCounter1 st)
    get st

exampleCounter1 :: Int
exampleCounter1 = runPureEff $ runCounter1 $ \c -> do
  incCounter1 c
  incCounter1 c
  incCounter1 c

-- > exampeleCounter1
-- 3

-- Counter 2

data Counter2 e1 e2 = MkCounter2 (State Int e1) (Exception () e2)

incCounter2 :: (e1 :> es, e2 :> es) => Counter2 e1 e2 -> Eff es ()
incCounter2 (MkCounter2 st ex) = do
  count <- get st
  when (count >= 10) $
    throw ex ()
  put st (count + 1)

runCounter2 ::
  (forall e1 e2. Counter2 e1 e2 -> Eff (e2 :& e1 :& es) r) ->
  Eff es Int
runCounter2 k =
  evalState 0 $ \st -> do
    _ <- try $ \ex -> do
      k (MkCounter2 st ex)
    get st

exampleCounter2 :: Int
exampleCounter2 = runPureEff $ runCounter2 $ \c ->
  forever $
    incCounter2 c

-- > exampleCounter2
-- 10

-- Counter 3

data Counter3 e = MkCounter3 (State Int e) (Exception () e)

incCounter3 :: (e :> es) => Counter3 e -> Eff es ()
incCounter3 (MkCounter3 st ex) = do
  count <- get st
  when (count >= 10) $
    throw ex ()
  put st (count + 1)

runCounter3 ::
  (forall e. Counter3 e -> Eff (e :& es) r) ->
  Eff es Int
runCounter3 k =
  evalState 0 $ \st -> do
    _ <- try $ \ex -> do
      useImplIn k (MkCounter3 (mapHandle st) (mapHandle ex))
    get st

exampleCounter3 :: Int
exampleCounter3 = runPureEff $ runCounter3 $ \c ->
  forever $
    incCounter3 c

-- > exampleCounter3
-- 10

-- Counter 4

data Counter4 e
  = MkCounter4 (State Int e) (Exception () e) (Stream String e)

incCounter4 :: (e :> es) => Counter4 e -> Eff es ()
incCounter4 (MkCounter4 st ex y) = do
  count <- get st

  when (even count) $
    yield y "Count was even"

  when (count >= 10) $
    throw ex ()

  put st (count + 1)

getCounter4 :: (e :> es) => Counter4 e -> String -> Eff es Int
getCounter4 (MkCounter4 st _ y) msg = do
  yield y msg
  get st

runCounter4 ::
  (e1 :> es) =>
  Stream String e1 ->
  (forall e. Counter4 e -> Eff (e :& es) r) ->
  Eff es Int
runCounter4 y k =
  evalState 0 $ \st -> do
    _ <- try $ \ex -> do
      useImplIn k (MkCounter4 (mapHandle st) (mapHandle ex) (mapHandle y))
    get st

exampleCounter4 :: ([String], Int)
exampleCounter4 = runPureEff $ yieldToList $ \y -> do
  runCounter4 y $ \c -> do
    incCounter4 c
    incCounter4 c
    n <- getCounter4 c "I'm getting the counter"
    when (n == 2) $
      yield y "n was 2, as expected"

-- > exampleCounter4
-- (["Count was even","I'm getting the counter","n was 2, as expected"],2)

-- Counter 5

data Counter5 e = MkCounter5
  { incCounter5Impl :: Eff e (),
    getCounter5Impl :: String -> Eff e Int
  }

incCounter5 :: (e :> es) => Counter5 e -> Eff es ()
incCounter5 e = useImpl (incCounter5Impl e)

getCounter5 :: (e :> es) => Counter5 e -> String -> Eff es Int
getCounter5 e msg = useImpl (getCounter5Impl e msg)

runCounter5 ::
  (e1 :> es) =>
  Stream String e1 ->
  (forall e. Counter5 e -> Eff (e :& es) r) ->
  Eff es Int
runCounter5 y k =
  evalState 0 $ \st -> do
    _ <- try $ \ex -> do
      useImplIn
        k
        ( MkCounter5
            { incCounter5Impl = do
                count <- get st

                when (even count) $
                  yield y "Count was even"

                when (count >= 10) $
                  throw ex ()

                put st (count + 1),
              getCounter5Impl = \msg -> do
                yield y msg
                get st
            }
        )
    get st

exampleCounter5 :: ([String], Int)
exampleCounter5 = runPureEff $ yieldToList $ \y -> do
  runCounter5 y $ \c -> do
    incCounter5 c
    incCounter5 c
    n <- getCounter5 c "I'm getting the counter"
    when (n == 2) $
      yield y "n was 2, as expected"

-- > exampleCounter5
-- (["Count was even","I'm getting the counter","n was 2, as expected"],2)

-- Counter 6

data Counter6 e = MkCounter6
  { incCounter6Impl :: Eff e (),
    counter6State :: State Int e,
    counter6Stream :: Stream String e
  }

incCounter6 :: (e :> es) => Counter6 e -> Eff es ()
incCounter6 e = useImpl (incCounter6Impl e)

getCounter6 :: (e :> es) => Counter6 e -> String -> Eff es Int
getCounter6 (MkCounter6 _ st y) msg = do
  yield y msg
  get st

runCounter6 ::
  (e1 :> es) =>
  Stream String e1 ->
  (forall e. Counter6 e -> Eff (e :& es) r) ->
  Eff es Int
runCounter6 y k =
  evalState 0 $ \st -> do
    _ <- try $ \ex -> do
      useImplIn
        k
        ( MkCounter6
            { incCounter6Impl = do
                count <- get st

                when (even count) $
                  yield y "Count was even"

                when (count >= 10) $
                  throw ex ()

                put st (count + 1),
              counter6State = mapHandle st,
              counter6Stream = mapHandle y
            }
        )
    get st

exampleCounter6 :: ([String], Int)
exampleCounter6 = runPureEff $ yieldToList $ \y -> do
  runCounter6 y $ \c -> do
    incCounter6 c
    incCounter6 c
    n <- getCounter6 c "I'm getting the counter"
    when (n == 2) $
      yield y "n was 2, as expected"

-- > exampleCounter6
-- (["Count was even","I'm getting the counter","n was 2, as expected"],2)

-- FileSystem

data FileSystem es = MkFileSystem
  { readFileImpl :: FilePath -> Eff es String,
    writeFileImpl :: FilePath -> String -> Eff es ()
  }

readFile :: (e :> es) => FileSystem e -> FilePath -> Eff es String
readFile fs filepath = useImpl (readFileImpl fs filepath)

writeFile :: (e :> es) => FileSystem e -> FilePath -> String -> Eff es ()
writeFile fs filepath contents = useImpl (writeFileImpl fs filepath contents)

runFileSystemPure ::
  (e1 :> es) =>
  Exception String e1 ->
  [(FilePath, String)] ->
  (forall e2. FileSystem e2 -> Eff (e2 :& es) r) ->
  Eff es r
runFileSystemPure ex fs0 k =
  evalState fs0 $ \fs ->
    useImplIn
      k
      MkFileSystem
        { readFileImpl = \path -> do
            fs' <- get fs
            case lookup path fs' of
              Nothing ->
                throw ex ("File not found: " <> path)
              Just s -> pure s,
          writeFileImpl = \path contents ->
            modify fs ((path, contents) :)
        }

runFileSystemIO ::
  forall e1 e2 es r.
  (e1 :> es, e2 :> es) =>
  Exception String e1 ->
  IOE e2 ->
  (forall e. FileSystem e -> Eff (e :& es) r) ->
  Eff es r
runFileSystemIO ex io k =
  useImplIn
    k
    MkFileSystem
      { readFileImpl =
          adapt . Prelude.readFile,
        writeFileImpl =
          \path -> adapt . Prelude.writeFile path
      }
  where
    adapt :: (e1 :> ess, e2 :> ess) => IO a -> Eff ess a
    adapt m =
      effIO io (Control.Exception.try @IOException m) >>= \case
        Left e -> throw ex (show e)
        Right r -> pure r

action :: (e :> es) => FileSystem e -> Eff es String
action fs = do
  file <- readFile fs "/dev/null"
  when (length file == 0) $ do
    writeFile fs "/tmp/bluefin" "Hello!\n"
  readFile fs "/tmp/doesn't exist"

exampleRunFileSystemPure :: Either String String
exampleRunFileSystemPure = runPureEff $ try $ \ex ->
  runFileSystemPure ex [("/dev/null", "")] action

-- > exampleRunFileSystemPure
-- Left "File not found: /tmp/doesn't exist"

exampleRunFileSystemIO :: IO (Either String String)
exampleRunFileSystemIO = runEff $ \io -> try $ \ex ->
  runFileSystemIO ex io action

-- > exampleRunFileSystemIO
-- Left "/tmp/doesn't exist: openFile: does not exist (No such file or directory)"
-- \$ cat /tmp/bluefin
-- Hello!

-- instance Handle example

data Application e = MkApplication
  { queryDatabase :: String -> Int -> Eff e [String],
    applicationState :: State (Int, Bool) e,
    logger :: Stream String e
  }

instance Handle Application where
  mapHandle
    MkApplication
      { queryDatabase = q,
        applicationState = a,
        logger = l
      } =
      MkApplication
        { queryDatabase = (fmap . fmap) useImpl q,
          applicationState = mapHandle a,
          logger = mapHandle l
        }

-- This example shows a case where we can use @bracket@ polymorphically
-- in order to perform correct cleanup if @es@ is instantiated to a
-- set of effects that includes exceptions.
polymorphicBracket ::
  (st :> es) =>
  State (Integer, Bool) st ->
  Eff es () ->
  Eff es ()
polymorphicBracket st act =
  bracket
    (pure ())
    -- Always set the boolean indicating that we have terminated
    (\_ -> modify st (\(c, b) -> (c, True)))
    -- Perform the given effectful action, then increment the counter
    (\_ -> do act; modify st (\(c, b) -> ((c + 1), b)))

-- Results in (1, True)
polymorphicBracketExample1 :: (Integer, Bool)
polymorphicBracketExample1 =
  runPureEff $ do
    (_res, st) <- runState (0, False) $ \st -> polymorphicBracket st (pure ())
    pure st

-- Results in (0, True)
polymorphicBracketExample2 :: (Integer, Bool)
polymorphicBracketExample2 =
  runPureEff $ do
    (_res, st) <- runState (0, False) $ \st -> try $ \e -> polymorphicBracket st (throw e 42)
    pure st

pipesExample1 :: IO ()
pipesExample1 = runEff $ \io -> runEffect (count >-> P.print io)
  where
    count :: (e :> es) => Producer Int e -> Eff es ()
    count p = for_ [1 .. 5] $ \i -> P.yield p i

pipesExample2 :: IO String
pipesExample2 = runEff $ \io -> runEffect $ do
  stdinLn io >-> takeWhile' (/= "quit") >-> stdoutLn io

-- Acquiring resource
-- 1
-- 2
-- 3
-- 4
-- 5
-- Releasing resource
-- Finishing
promptCoroutine :: IO ()
promptCoroutine = runEff $ \io -> do
  -- consumeStream connects a consumer to a producer
  consumeStream
    -- Like a pipes Consumer.  Prints the first five elements it
    -- awaits.
    ( \r -> for_ [1 :: Int .. 5] $ \_ -> do
        v <- await r
        effIO io (print v)
    )
    -- Like a pipes Producer. Yields successive integers indefinitely.
    -- Unlike in pipes, we can simply use Bluefin's standard bracket
    -- for prompt release of a resource
    ( \y ->
        bracket
          (effIO io (putStrLn "Acquiring resource"))
          (\_ -> effIO io (putStrLn "Releasing resource"))
          (\_ -> for_ [1 :: Int ..] $ \i -> yield y i)
    )
  effIO io (putStrLn "Finishing")

rethrowIOExample :: IO ()
rethrowIOExample = runEff $ \io -> do
  r <- try $ \ex -> do
    rethrowIO @Control.Exception.IOException io ex $ do
      effIO io (Prelude.readFile "/tmp/doesnt-exist")

  effIO io $ putStrLn $ case r of
    Left e -> "Caught IOException:\n" ++ show e
    Right contents -> contents

chunksOfBS ::
  (e1 :> es, e2 :> es) =>
  Int ->
  Consume BS.ByteString e1 ->
  Stream BS.ByteString e2 ->
  Eff es r
chunksOfBS n c y = evalState BS.empty $
  \leftovers -> forever $ do
    leftovers' <- get leftovers
    let (possibleChunk, rest) = BS.splitAt n leftovers'
    let lenPossibleChunk = BS.length possibleChunk
    if
      | lenPossibleChunk < n -> do
          more <- await c
          put leftovers (leftovers' <> more)
      | lenPossibleChunk == n -> do
          yield y possibleChunk
          put leftovers rest
      | otherwise -> error "Impossible"

runExample ::
  (forall e es. Stream String e -> Eff (e :& es) ()) ->
  IO ()
runExample lines_ = runEff $ \io -> forEach lines_ $ \line -> do
  effIO io (putStrLn line)

exampleChunksOfBS :: (e :> es) => Stream String e -> Eff es ()
exampleChunksOfBS line = do
  forEach
    ( \y -> do
        consumeStream
          (\c -> chunksOfBS 4 c y)
          ( \y' -> do
              for_ ["Hello", " ", "world", "!!!"] $
                yield y'
          )
    )
    (yield line . C8.unpack)

chunkBS ::
  (e :> es) =>
  Int ->
  BS.ByteString ->
  Stream BS.ByteString e ->
  Eff es ()
chunkBS n bs y =
  consumeStream
    (\c -> chunksOfBS n c y)
    (\y' -> yield y' bs)

exampleChunkBS :: (e :> es) => Stream String e -> Eff es ()
exampleChunkBS line = do
  let msg = "Hello world!!!"
  forEach (chunkBS 4 msg) (yield line . C8.unpack)

pairUp ::
  (e1 :> es, e2 :> es) =>
  Consume a e1 ->
  Stream (a, a) e2 ->
  Eff es void
pairUp c y = forever $ do
  c1 <- await c
  c2 <- await c
  yield y (c1, c2)

examplePairUp :: (e :> es) => Stream String e -> Eff es ()
examplePairUp line =
  forEach
    ( \y -> do
        consumeStream
          (\c -> pairUp c y)
          (\y' -> for_ [1 :: Int .. 10] (yield y'))
    )
    (yield line . show)

interleave ::
  (e1 :> es, e2 :> es, e3 :> es) =>
  Consume a e1 ->
  Consume a e2 ->
  Stream a e3 ->
  Eff es void
interleave c1 c2 y = forever $ do
  c1' <- await c1
  yield y c1'
  c2' <- await c2
  yield y c2'

exampleInterleave :: (e1 :> es) => Stream String e1 -> Eff es ()
exampleInterleave line =
  forEach
    ( \y -> do
        consumeStream
          ( \c2 ->
              consumeStream
                ( \c1 ->
                    interleave c1 c2 y
                )
                (\y' -> for_ [1 :: Int .. 5] (yield y'))
          )
          (\y' -> for_ [10 :: Int .. 15] (yield y'))
    )
    (yield line . show)

instructions ::
  (e1 :> es, e2 :> es, e3 :> es) =>
  Consume BS.ByteString e1 ->
  Stream BS.ByteString e2 ->
  IOE e3 ->
  Eff es void
instructions pages insns io = do
  let pagePairs = pairUp pages

  let words' word = do
        forEach pagePairs $ \(p1, p2) -> do
          let p1Words = chunkBS 16 p1
          let p2Words = chunkBS 16 p2

          consumeStream
            ( \c2 ->
                consumeStream
                  ( \c1 -> do
                      forEach (interleave c1 c2) $ \w -> do
                        yield word w
                        effIO io (putStrLn ("Debugging tracing " ++ C8.unpack w))
                  )
                  p1Words
            )
            p2Words

  consumeStream (\c -> chunksOfBS 3 c insns) words'

-- ghci> exampleRunInstructions
-- ABC
-- DEF
-- GHI
-- JKL
-- 123
-- Debugging tracing ABCDEFGHIJKL1234
-- 4QR
-- STU
-- VWX
-- YZ0
-- 987
-- Debugging tracing QRSTUVWXYZ098765
-- 65A
-- BCD
-- EFG
-- HIJ
-- KL1
-- 234
-- Debugging tracing ABCDEFGHIJKL1234
-- QRS
-- TUV
-- WXY
-- Z09
-- 876
-- Debugging tracing QRSTUVWXYZ098765
exampleRunInstructions :: IO ()
exampleRunInstructions = runEff $ \io -> do
  forEach
    ( \y ->
        consumeStream
          (\c -> instructions c y io)
          ( \y' -> replicateM_ 2 $ do
              yield y' "ABCDEFGHIJKL1234567890"
              yield y' "QRSTUVWXYZ0987654321"
          )
    )
    (effIO io . putStrLn . C8.unpack)

zipWithConsume ::
  (e1 :> es, e2 :> es, e :> es) =>
  (a1 -> a2 -> Eff es b) ->
  Consume a1 e1 ->
  Consume a2 e2 ->
  Stream b e ->
  Eff es void
zipWithConsume f c1 c2 y = forever $ do
  a1 <- await c1
  a2 <- await c2
  yield y =<< f a1 a2

zipWithErrorMismatch ::
  (e1 :> es, e2 :> es, e3 :> es, e :> es) =>
  (a1 -> a2 -> Eff es b) ->
  Exception () e3 ->
  Consume (Maybe a1) e1 ->
  Consume (Maybe a2) e2 ->
  Stream b e ->
  Eff es ()
zipWithErrorMismatch f err c1 c2 y = withJump $ \done -> do
  let g (Just a1) (Just a2) = useImpl (f a1 a2)
      g Nothing Nothing = jumpTo done
      g (Just _) Nothing = jumpTo err
      g Nothing (Just _) = jumpTo err

  zipWithConsume g c1 c2 y

zipWithErrorMismatch' ::
  (e3 :> es, e :> es) =>
  (a1 -> a2 -> Eff es b) ->
  Exception () e3 ->
  (forall e'. Stream a1 e' -> Eff (e' :& es) ()) ->
  (forall e'. Stream a2 e' -> Eff (e' :& es) ()) ->
  Stream b e ->
  Eff es ()
zipWithErrorMismatch' f err k1 k2 y = do
  consumeStream
    ( \c1 ->
        consumeStream
          ( \c2 ->
              zipWithErrorMismatch (\x' y' -> useImpl (f x' y')) err c1 c2 y
          )
          (foo (useImplWithin k2))
    )
    (foo (useImplWithin k1))

exampleZipWithErrorMismatch' :: (e :> es) => Stream String e -> Eff es ()
exampleZipWithErrorMismatch' y = do
  let k1 y' = for_ [1 .. 5 :: Int] (yield y')
  let k2 y' = for_ ['A' .. 'E'] (yield y')

  r <- try $ \ex ->
    forEach
      (zipWithErrorMismatch' (\a b_ -> pure (a, b_)) ex k1 k2)
      (yield y . show)

  yield y (show r)

foo ::
  (e1 :> es) =>
  (forall e. Stream a e -> Eff (e :& es) ()) ->
  Stream (Maybe a) e1 ->
  Eff es void
foo k y = do
  forEach k $ \a -> do
    yield y (Just a)

  forever $ do
    yield y Nothing

zipWithCheckLength :: (a -> b -> r) -> [a] -> [b] -> [r]
zipWithCheckLength f as bs =
  untilNothing g (zip (extend as) (extend bs))
  where
    g (Just a_, Just b_) = Just (f a_ b_)
    g (Nothing, Nothing) = Nothing
    g (Just _, Nothing) = error msg
    g (Nothing, Just _) = error msg

    extend = (++ repeat Nothing) . map Just
    msg = "Unequal lengths"

untilNothing :: (a -> Maybe b) -> [a] -> [b]
untilNothing _ [] = []
untilNothing f (x : xs) = case f x of
  Nothing -> []
  Just b_ -> b_ : untilNothing f xs
