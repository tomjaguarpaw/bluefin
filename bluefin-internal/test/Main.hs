{-# LANGUAGE NoMonoLocalBinds #-}

module Main (main) where

import Bluefin.Internal
import Control.Monad (unless, when)
import Data.Foldable (for_)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Prelude hiding (break, read)

main :: IO ()
main = do
  allTrue $ \y -> do
    let assert n c = yield y (n, if c then Nothing else Just (Forall (Nest (\_ -> pure ()))))
    let assertEqual n c1 c2 =
          yield
            y
            ( n,
              if c1 == c2
                then Nothing
                else
                  Just
                    ( Forall
                        ( Nest
                            ( \y2 -> do
                                yield y2 ("Expected: " ++ show c1)
                                yield y2 ("But got: " ++ show c2)
                            )
                        )
                    )
            )

    assertEqual "oddsUntilFirstGreaterThan5" oddsUntilFirstGreaterThan5 [1, 3, 5, 7]
    assertEqual "index 1" ([0, 1, 2, 3] !? 2) (Just 2)
    assertEqual "index 2" ([0, 1, 2, 3] !? 4) Nothing
    assertEqual
      "Exception 1"
      (runEff (try (eitherEff (Left True))))
      (Left True :: Either Bool ())
    assertEqual
      "Exception 2"
      (runEff (try (eitherEff (Right True))))
      (Right True :: Either () Bool)
    assertEqual
      "State"
      (runEff (runState 10 (stateEff (\n -> (show n, n * 2)))))
      ("10", 20)
    assertEqual
      "List"
      (runEff (yieldToList (listEff ([20, 30, 40], "Hello"))))
      ([20, 30, 40], "Hello")

newtype Nest h effs = Nest
  { unNest ::
      forall e4.
      (e4 :> effs) =>
      h e4 ->
      Eff effs ()
  }

newtype Forall f = Forall {unForall :: forall e. f e}

runTests ::
  forall effs e3.
  (e3 :> effs) =>
  ( forall e1 e2.
    Stream
      ( String,
        Maybe
          ( Forall (Nest (Stream String))
          )
      )
      e1 ->
    Eff (e1 :& e2 :& effs) ()
  ) ->
  Stream String e3 ->
  Eff effs Bool
runTests f y = do
  evalState True $ \passedAllSoFar -> do
    forEach f $ \(name, passedThisOne) -> do
      case passedThisOne of
        Just _ -> put passedAllSoFar False
        Nothing -> pure ()

      let mark = case passedThisOne of
            Nothing -> "✓"
            Just _ -> "✗"

      yield y (mark ++ " " ++ name)

      case passedThisOne of
        Nothing -> pure ()
        Just (Forall n) -> do
          yield y ""
          forEach (unNest n) $ \entry -> do
            yield y ("    " ++ entry)
          yield y ""

    get passedAllSoFar

allTrue ::
  (forall e1 effs. Stream (String, Maybe (Forall (Nest (Stream String)))) e1 -> Eff (e1 :& effs) ()) ->
  IO ()
allTrue f = runEffIO $ \ioe -> do
  passed <- forEach (runTests f) $ \text ->
    effIO ioe (putStrLn text)

  effIO ioe $ case passed of
    True -> pure ()
    False -> exitWith (ExitFailure 1)

(!?) :: [a] -> Int -> Maybe a
xs !? i = runEff $
  withEarlyReturn $ \ret -> do
    evalState 0 $ \s -> do
      for_ xs $ \a -> do
        i' <- get s
        when (i == i') (returnEarly ret (Just a))
        put s (i' + 1)
    pure Nothing

oddsUntilFirstGreaterThan5 :: [Int]
oddsUntilFirstGreaterThan5 =
  fst $
    runEff $
      yieldToList $ \y -> do
        withJump $ \break -> do
          for_ [1 .. 10] $ \i -> do
            withJump $ \continue -> do
              when (i `mod` 2 == 0) $
                jumpTo continue
              yield y i
              when (i > 5) $
                jumpTo break

-- | Inverse to 'try'
eitherEff :: (e1 :> effs) => Either e r -> Exception e e1 -> Eff effs r
eitherEff eith ex = case eith of
  Left e -> throw ex e
  Right r -> pure r

-- | Inverse to 'runState'
stateEff :: (e1 :> effs) => (s -> (a, s)) -> State s e1 -> Eff effs a
stateEff f st = do
  s <- get st
  let (a, s') = f s
  put st s'
  pure a

-- | Inverse to 'yieldToList'
listEff :: (e1 :> effs) => ([a], r) -> Stream a e1 -> Eff effs r
listEff (as, r) y = do
  for_ as (yield y)
  pure r
