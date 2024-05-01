{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Bluefin.Internal.Examples where

import Bluefin.Internal hiding (w)
import Control.Exception (IOException)
import qualified Control.Exception
import Control.Monad (forever, unless, when)
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
