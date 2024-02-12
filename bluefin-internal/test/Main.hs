{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Bluefin.Internal
import Control.Monad (when)
import Data.Foldable (for_)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Prelude hiding (break, read)

main :: IO ()
main = do
  allTrue $ \y -> do
    let assertEqual' = assertEqual y

    assertEqual' "oddsUntilFirstGreaterThan5" oddsUntilFirstGreaterThan5 [1, 3, 5, 7]
    assertEqual' "index 1" ([0, 1, 2, 3] !? 2) (Just 2)
    assertEqual' "index 2" ([0, 1, 2, 3] !? 4) Nothing
    assertEqual'
      "Exception 1"
      (runEff (try (eitherEff (Left True))))
      (Left True :: Either Bool ())
    assertEqual'
      "Exception 2"
      (runEff (try (eitherEff (Right True))))
      (Right True :: Either () Bool)
    assertEqual'
      "State"
      (runEff (runState 10 (stateEff (\n -> (show n, n * 2)))))
      ("10", 20)
    assertEqual'
      "List"
      (runEff (yieldToList (listEff ([20, 30, 40], "Hello"))))
      ([20, 30, 40], "Hello")

-- A SpecH yields pairs of
--
--   (name, Maybe (stream of error text))
type SpecH effs = Stream (String, Maybe (Nest (Stream String) Eff effs ()))

-- I'm still not convinced that this scheme is practical for calling
-- outer effects from the inner.  The problem is that at the time of
-- interpretation some outer effects are unavailable because they have
-- already been handled (for example some state which the test cases
-- use) or, in the case of the Stream effect itself, because they are
-- currently being handled (we can't yield more results to the Stream
-- whilst we're handling it).
--
-- It seems likely that with a lot of awkwardness we can arrange for
-- the type parameters to be compatible with the order of handling,
-- but then we've coupled the order of the handlers to the effectful
-- operation, which is antithetical to the point of Bluefin.
assertEqual ::
  (e1 :> e1effs, Eq a, Show a) => SpecH effs e1 -> String -> a -> a -> Eff (e1effs :& effs) ()
assertEqual y n c1 c2 =
  pushFirst $
    yield
      y
      ( n,
        if c1 == c2
          then Nothing
          else
            Just
              ( p
                  ( \y2 -> do
                      yield y2 ("Expected: " ++ show c1)
                      yield y2 ("But got: " ++ show c2)
                  )
              )
      )

p ::
  (forall e effs'. (e :> effs') => h e -> Eff effs' r) ->
  Nest h Eff effs r
p bb = Nest (pushFirst . bb)

newtype Nest h t effs r = Nest
  { unNest ::
      forall e effs'.
      (e :> effs') =>
      h e ->
      t (effs' :& effs) r
  }

runTests ::
  forall effs e3.
  (e3 :> effs) =>
  (forall e1 e2. SpecH (e2 :& effs) e1 -> Eff (e1 :& e2 :& effs) ()) ->
  Stream String e3 ->
  Eff effs Bool
runTests f y = do
  evalState True $ \(passedAllSoFar :: State Bool e2) -> do
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
        Just n -> do
          yield y "" :: Eff (e2 :& effs) ()
          _ <- forEach (unNest n) $ \entry -> do
            yield y ("    " ++ entry)
          yield y ""

    get passedAllSoFar

allTrue ::
  (forall e1 effs. SpecH effs e1 -> Eff (e1 :& effs) ()) ->
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
