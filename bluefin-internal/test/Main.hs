module Main (main) where

import Bluefin.Internal
import Control.Monad (unless, when)
import Data.Foldable (for_)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Prelude hiding (break, read)

main :: IO ()
main = do
  allTrue $ \y -> do
    let assert n c = yield y (n, c)

    assert "oddsUntilFirstGreaterThan5" (oddsUntilFirstGreaterThan5 == [1, 3, 5, 7])
    assert "index 1" ([0, 1, 2, 3] !? 2 == Just 2)
    assert "index 2" ([0, 1, 2, 3] !? 4 == Nothing)
    assert
      "Exception 1"
      (runEff (handleException (eitherEff (Left True))) == (Left True :: Either Bool ()))
    assert
      "Exception 2"
      (runEff (handleException (eitherEff (Right True))) == (Right True :: Either () Bool))
    assert
      "State"
      (runEff (runState 10 (stateEff (\n -> (show n, n * 2)))) == ("10", 20))

allTrue ::
  (forall e1 effs. Stream (String, Bool) e1 -> Eff (e1 :& effs) ()) ->
  IO ()
allTrue f = runEffIO $ \ioe -> do
  passed <- evalState True $ \passedAllSoFar -> do
    forEach f $ \(name, passedThisOne) -> do
      unless passedThisOne $
        write passedAllSoFar False

      effIO
        ioe
        ( putStrLn
            ( name
                ++ " "
                ++ if passedThisOne
                  then "PASS"
                  else "FAIL"
            )
        )
    read passedAllSoFar

  effIO ioe $ case passed of
    True -> pure ()
    False -> exitWith (ExitFailure 1)

(!?) :: [a] -> Int -> Maybe a
xs !? i = runEff $
  withEarlyReturn $ \ret -> do
    evalState 0 $ \s -> do
      for_ xs $ \a -> do
        i' <- read s
        when (i == i') (earlyReturn ret (Just a))
        write s (i' + 1)
    earlyReturn ret Nothing

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

eitherEff :: (e1 :> effs) => Either e r -> Exception e e1 -> Eff effs r
eitherEff eith ex = case eith of
  Left e -> throw ex e
  Right r -> pure r

stateEff :: (e1 :> effs) => (s -> (a, s)) -> State s e1 -> Eff effs a
stateEff f st = do
  s <- read st
  let (a, s') = f s
  write st s'
  pure a
