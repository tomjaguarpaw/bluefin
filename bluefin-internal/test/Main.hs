{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Bluefin.Internal
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Monoid (All (All))
import System.Exit (ExitCode (ExitFailure), exitWith)
import Prelude hiding (break, read)

main :: IO ()
main = runEff_ $ \io -> do
  runSpecH io $ \y -> do
    let assertEqual' = assertEqual y

    assertEqual' "oddsUntilFirstGreaterThan5" oddsUntilFirstGreaterThan5 [1, 3, 5, 7]
    assertEqual' "index 1" ([0, 1, 2, 3] !? 2) (Just 2)
    assertEqual' "index 2" ([0, 1, 2, 3] !? 4) Nothing
    assertEqual'
      "Exception 1"
      (runPureEff (try (eitherEff (Left True))))
      (Left True :: Either Bool ())
    assertEqual'
      "Exception 2"
      (runPureEff (try (eitherEff (Right True))))
      (Right True :: Either () Bool)
    assertEqual'
      "State"
      (runPureEff (runState 10 (stateEff (\n -> (show n, n * 2)))))
      ("10", 20)
    assertEqual'
      "List"
      (runPureEff (yieldToList (listEff ([20, 30, 40], "Hello"))))
      ([20, 30, 40], "Hello")

    test_localInHandler y

-- A SpecH yields pairs of
--
--   (name, Maybe (stream of error text))
type SpecH = Stream (String, Maybe (SpecInfo ()))

assertEqual ::
  (e :> es, Eq a, Show a) => SpecH e -> String -> a -> a -> Eff es ()
assertEqual y n c1 c2 =
  yield
    y
    ( n,
      if c1 == c2
        then Nothing
        else Just $ withSpecInfo $ \y2 -> do
          yield y2 ("Expected: " ++ show c1)
          yield y2 ("But got: " ++ show c2)
    )

type SpecInfo r = Forall (Stream String :~> WrapEff r)

runSpecInfo :: SpecInfo r -> Coroutine String () e -> WrapEff r (e :& es)
runSpecInfo s = unNest (unForall s)

withSpecInfo ::
  (forall e es. Stream String e -> Eff (e :& es) r) ->
  SpecInfo r
withSpecInfo x =
  Forall (Nest (\s -> MkWrapEff (x s)))

newtype (h :~> t) es = Nest {unNest :: forall e. h e -> t (e :& es)}

newtype Forall t = Forall {unForall :: forall es. t es}

newtype WrapEff r es = MkWrapEff {unWrapEff :: Eff es r}

instance Handle (WrapEff r) where
  mapHandle (MkWrapEff e) = MkWrapEff (useImpl e)

runTests ::
  forall es e3.
  (e3 :> es) =>
  (forall e1. SpecH e1 -> Eff (e1 :& es) ()) ->
  Stream String e3 ->
  Eff es Bool
runTests f y = do
  ((), All passedAll) <- runWriter $ \passedAllSoFar -> do
    forEach (useImplUnder . f) $ \(name, mFailure) -> do
      let passed = case mFailure of
            Just _ -> False
            Nothing -> True

      tell passedAllSoFar (All passed)

      let mark = if passed then "✓" else "✗"

      yield y (mark ++ " " ++ name)

      case mFailure of
        Nothing -> pure ()
        Just n -> do
          yield y "" :: Eff (e2 :& es) ()
          _ <- forEach (\h -> unWrapEff (runSpecInfo n h)) $ \entry -> do
            yield y ("    " ++ entry)
          yield y ""

  pure passedAll

runSpecH ::
  (e :> es) =>
  IOE e ->
  (forall e1. SpecH e1 -> Eff (e1 :& es) ()) ->
  Eff es ()
runSpecH ioe f = do
  passed <- forEach (runTests (useImplUnder . f)) $ \text ->
    effIO ioe (putStrLn text)

  effIO ioe $ case passed of
    True -> pure ()
    False -> exitWith (ExitFailure 1)

(!?) :: [a] -> Int -> Maybe a
xs !? i = runPureEff $
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
    runPureEff $
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
eitherEff :: (e1 :> es) => Either e r -> Exception e e1 -> Eff es r
eitherEff eith ex = case eith of
  Left e -> throw ex e
  Right r -> pure r

-- | Inverse to 'runState'
stateEff :: (e1 :> es) => (s -> (a, s)) -> State s e1 -> Eff es a
stateEff f st = do
  s <- get st
  let (a, s') = f s
  put st s'
  pure a

-- | Inverse to 'yieldToList'
listEff :: (e1 :> es) => ([a], r) -> Stream a e1 -> Eff es r
listEff (as, r) y = do
  for_ as (yield y)
  pure r

test_localInHandler :: (e :> es) => SpecH e -> Eff es ()
test_localInHandler y = runReader "global" $ \re ->
  forEach
    (\y2 -> local re (const "local") (yield y2 ()))
    (\() -> assertEqual y "Reader local" "local" =<< ask re)
