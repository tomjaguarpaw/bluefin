module Test.SpecH where

import System.Exit (ExitCode (ExitFailure), exitWith)
import Bluefin.Internal
import Bluefin.Internal.DslBuilder
import Data.Monoid (All (All))

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
        else Just $ dslBuilder $ \y2 -> do
          yield y2 ("Expected: " ++ show c1)
          yield y2 ("But got: " ++ show c2)
    )

type SpecInfo r = DslBuilder (Stream String) r

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
          _ <- forEach (useImpl . flip runDslBuilder n) $ \entry -> do
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
