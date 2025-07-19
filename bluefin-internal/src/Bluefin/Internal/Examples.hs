{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Bluefin.Internal.Examples where

import Bluefin.Internal hiding (b, w)
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
import Data.Foldable (for_)
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
monadIOExample = runEff_ $ \io -> withMonadIO io $ liftIO $ do
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
withYieldToListExample = runPureEff $ withYieldToList @Int $ \y -> do
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
  for_ [0 .. 4] $ \i -> do
    yield y i
    yield y (i * 10)

ignoreStreamExample :: Int
ignoreStreamExample = runPureEff $ ignoreStream @Int $ \y -> do
  for_ [0 .. 4] $ \i -> do
    yield y i
    yield y (i * 10)

  pure 42

-- ([1,2,3,1,2,3],())
cycleToStreamExample :: ([Int], ())
cycleToStreamExample = runPureEff $ yieldToList $ \yOut -> do
  consumeStream
    (\c -> takeConsume 6 c yOut)
    (\yIn -> cycleToStream [1..3] yIn)

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
effIOExample = runEff_ $ \io -> do
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
example3_ = runEff_ $ \io -> do
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
        consumeEach (useImplUnder . k) $ do
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
  withJump $ \done -> evalState n $ \s -> consumeEach (useImplUnder . k) $ do
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
          useImplUnder . x

awaitExample :: IO ()
awaitExample = runEff_ $ \io -> do
  awaitList [1 :: Int ..] io $ awaitUsage io $ \rec -> do
    replicateM_ 5 (await rec)

consumeStreamExample :: IO (Either String String)
consumeStreamExample = runEff_ $ \io -> do
  try $ \ex -> do
    consumeStream
      ( \r ->
          bracket
            (effIO io (putStrLn "Starting 2"))
            (\_ -> effIO io (putStrLn "Leaving 2"))
            $ \_ -> do
              for_ [1 :: Int .. 100] $ \n -> do
                b <- await r
                effIO
                  io
                  ( putStrLn
                      ("Consumed body " ++ show b ++ " at time " ++ show n)
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
consumeStreamExample2 = runEff_ $ \io -> do
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
connectExample = runEff_ $ \io -> do
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
                b <- await r
                effIO
                  io
                  ( putStrLn
                      ("Consumed body " ++ show b ++ " at time " ++ show n)
                  )
              pure "Consumer finished first"
      )

zipCoroutinesExample :: IO ()
zipCoroutinesExample = runEff_ $ \io -> do
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
countExample = runEff_ $ \io -> do
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
  for_ [1 :: Int .. 10] $ \_ -> tell w (Any True)

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
runIncrementReadLine = runEff_ $ \io -> do
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

-- > exampleCounter1
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

-- Counter 3B

newtype Counter3B e = MkCounter3B (IOE e)

incCounter3B :: (e :> es) => Counter3B e -> Eff es ()
incCounter3B (MkCounter3B io) =
  effIO io (putStrLn "You tried to increment the counter")

runCounter3B ::
  (e1 :> es) =>
  IOE e1 ->
  (forall e. Counter3B e -> Eff (e :& es) r) ->
  Eff es r
runCounter3B io k = useImplIn k (MkCounter3B (mapHandle io))

exampleCounter3B :: IO ()
exampleCounter3B = runEff_ $ \io -> runCounter3B io $ \c -> do
  incCounter3B c
  incCounter3B c
  incCounter3B c

-- ghci> exampleCounter3B
-- You tried to increment the counter
-- You tried to increment the counter
-- You tried to increment the counter

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
  { incCounter5Impl :: forall e'. Eff (e' :& e) (),
    getCounter5Impl :: forall e'. String -> Eff (e' :& e) Int
  }

instance Handle Counter5 where
  mapHandle c =
    MkCounter5
      { incCounter5Impl = useImplUnder (incCounter5Impl c),
        getCounter5Impl = \msg -> useImplUnder (getCounter5Impl c msg)
      }

incCounter5 :: (e :> es) => Counter5 e -> Eff es ()
incCounter5 e = makeOp (incCounter5Impl (mapHandle e))

getCounter5 :: (e :> es) => Counter5 e -> String -> Eff es Int
getCounter5 e msg = makeOp (getCounter5Impl (mapHandle e) msg)

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
  { incCounter6Impl :: forall e'. Eff (e' :& e) (),
    counter6State :: State Int e,
    counter6Stream :: Stream String e
  }

instance Handle Counter6 where
  mapHandle c =
    MkCounter6
      { incCounter6Impl = useImplUnder (incCounter6Impl c),
        counter6State = mapHandle (counter6State c),
        counter6Stream = mapHandle (counter6Stream c)
      }

incCounter6 :: (e :> es) => Counter6 e -> Eff es ()
incCounter6 e = makeOp (incCounter6Impl (mapHandle e))

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

-- Counter 7

data Counter7 e = MkCounter7
  { incCounter7Impl :: forall e'. Exception () e' -> Eff (e' :& e) (),
    counter7State :: State Int e,
    counter7Stream :: Stream String e
  }

instance Handle Counter7 where
  mapHandle c =
    MkCounter7
      { incCounter7Impl = \ex -> useImplUnder (incCounter7Impl c ex),
        counter7State = mapHandle (counter7State c),
        counter7Stream = mapHandle (counter7Stream c)
      }

incCounter7 ::
  (e :> es, e1 :> es) => Counter7 e -> Exception () e1 -> Eff es ()
incCounter7 e ex = makeOp (incCounter7Impl (mapHandle e) (mapHandle ex))

getCounter7 :: (e :> es) => Counter7 e -> String -> Eff es Int
getCounter7 (MkCounter7 _ st y) msg = do
  yield y msg
  get st

runCounter7 ::
  (e1 :> es) =>
  Stream String e1 ->
  (forall e. Counter7 e -> Eff (e :& es) r) ->
  Eff es Int
runCounter7 y k =
  evalState 0 $ \st -> do
    _ <-
      useImplIn
        k
        ( MkCounter7
            { incCounter7Impl = \ex -> do
                count <- get st

                when (even count) $
                  yield y "Count was even"

                when (count >= 10) $
                  throw ex ()

                put st (count + 1),
              counter7State = mapHandle st,
              counter7Stream = mapHandle y
            }
        )
    get st

exampleCounter7A :: ([String], Int)
exampleCounter7A = runPureEff $ yieldToList $ \y -> do
  handle (\() -> pure (-42)) $ \ex ->
    runCounter7 y $ \c -> do
      incCounter7 c ex
      incCounter7 c ex
      n <- getCounter7 c "I'm getting the counter"
      when (n == 2) $
        yield y "n was 2, as expected"

-- > exampleCounter7A
-- (["Count was even","I'm getting the counter","n was 2, as expected"],2)

exampleCounter7B :: ([String], Int)
exampleCounter7B = runPureEff $ yieldToList $ \y -> do
  handle (\() -> pure (-42)) $ \ex ->
    runCounter7 y $ \c -> do
      forever (incCounter7 c ex)

-- > exampleCounter7B
-- (["Count was even","Count was even","Count was even","Count was even","Count was even","Count was even"],-42)

-- FileSystem

data FileSystem es = MkFileSystem
  { readFileImpl :: forall e. FilePath -> Eff (e :& es) String,
    writeFileImpl :: forall e. FilePath -> String -> Eff (e :& es) ()
  }

instance Handle FileSystem where
  mapHandle fs =
    MkFileSystem
      { readFileImpl = \fp -> useImplUnder (readFileImpl fs fp),
        writeFileImpl = \fp s -> useImplUnder (writeFileImpl fs fp s)
      }

readFile :: (e :> es) => FileSystem e -> FilePath -> Eff es String
readFile fs filepath = makeOp (readFileImpl (mapHandle fs) filepath)

writeFile :: (e :> es) => FileSystem e -> FilePath -> String -> Eff es ()
writeFile fs filepath contents =
  makeOp (writeFileImpl (mapHandle fs) filepath contents)

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
exampleRunFileSystemIO = runEff_ $ \io -> try $ \ex ->
  runFileSystemIO ex io action

-- > exampleRunFileSystemIO
-- Left "/tmp/doesn't exist: openFile: does not exist (No such file or directory)"
-- \$ cat /tmp/bluefin
-- Hello!

-- instance Handle example

data Application e = MkApplication
  { queryDatabase :: forall e'. String -> Int -> Eff (e' :& e) [String],
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
        { queryDatabase = \s i -> useImplUnder (q s i),
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
    (\_ -> modify st (\(c, _b) -> (c, True)))
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
    (_res, st) <- runState (0, False) $ \st -> try @Int $ \e -> polymorphicBracket st (throw e 42)
    pure st

pipesExample1 :: IO ()
pipesExample1 = runEff_ $ \io -> runEffect (count >-> P.print io)
  where
    count :: (e :> es) => Producer Int e -> Eff es ()
    count p = for_ [1 .. 5] $ \i -> P.yield p i

pipesExample2 :: IO String
pipesExample2 = runEff_ $ \io -> runEffect $ do
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
promptCoroutine = runEff_ $ \io -> do
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
rethrowIOExample = runEff_ $ \io -> do
  r <- try $ \ex -> do
    rethrowIO @Control.Exception.IOException io ex $ do
      effIO io (Prelude.readFile "/tmp/doesnt-exist")

  effIO io $ putStrLn $ case r of
    Left e -> "Caught IOException:\n" ++ show e
    Right contents -> contents

data DynamicReader r e = DynamicReader
  { askLRImpl :: forall e'. Eff (e' :& e) r,
    localLRImpl :: forall e' a. (r -> r) -> Eff e' a -> Eff (e' :& e) a
  }

instance Handle (DynamicReader r) where
  mapHandle h =
    DynamicReader
      { askLRImpl = useImplUnder (askLRImpl h),
        localLRImpl = \f k -> useImplUnder (localLRImpl h f k)
      }

askLR ::
  (e :> es) =>
  DynamicReader r e ->
  Eff es r
askLR c = makeOp (askLRImpl (mapHandle c))

localLR ::
  (e :> es) =>
  DynamicReader r e ->
  (r -> r) ->
  Eff es a ->
  Eff es a
localLR c f m = makeOp (localLRImpl (mapHandle c) f m)

runDynamicReader ::
  r ->
  (forall e. DynamicReader r e -> Eff (e :& es) a) ->
  Eff es a
runDynamicReader r k =
  runReader r $ \h -> do
    useImplIn
      k
      DynamicReader
        { askLRImpl = ask h,
          localLRImpl = \f k' -> makeOp (local h f (useImpl k'))
        }
