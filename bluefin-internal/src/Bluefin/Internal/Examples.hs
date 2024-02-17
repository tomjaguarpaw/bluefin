{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Bluefin.Internal.Examples where

import Bluefin.Internal
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Prelude hiding (drop, head, read, return)

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

type MyHandle = Compound (State Int) (Exception String)

myInc :: (e :> effs) => MyHandle e -> Eff effs ()
myInc h = withCompound h (\s _ -> modify s (+ 1))

myBail :: (e :> effs) => MyHandle e -> Eff effs r
myBail h = withCompound h $ \s e -> do
  i <- get s
  throw e ("Current state was: " ++ show i)

runMyHandle ::
  (forall e. MyHandle e -> Eff (e :& effs) a) ->
  Eff effs (Either String (a, Int))
runMyHandle f =
  try $ \e -> do
    runState 0 $ \s -> do
      runCompound s e f
