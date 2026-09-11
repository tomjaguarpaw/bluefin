{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Bluefin.Internal
import Control.Monad (forever, when)
import Data.Foldable (for_)
import Test.GeneralBracket (test_generalBracket)
import Test.RunPureEff
  ( InterruptedBracketResult (..),
    assertInterruptedBracketOutcome,
  )
import Test.SpecH (SpecH, assertEqual, runSpecH)
import Prelude hiding (break, read)

main :: IO ()
main = runEff $ \io -> do
  runSpecH io $ \y -> do
    let assertEqual' = assertEqual y

    assertInterruptedBracketOutcome
      io
      y
      "runPureEff retains bracket's rethrown exception"
      ThunkPoisoned
      runPureEff
    assertInterruptedBracketOutcome
      io
      y
      "runPureEffAsyncSafe survives an interrupted bracket"
      ThunkClean
      runPureEffAsyncSafe

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
    test_generalBracket io y
    test_streamConsumeReader y
    test_streamConsumeHandleReader y
    test_unliftIOReader io y

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
eitherEff :: (e1 <: es) => Either e r -> Exception e e1 -> Eff es r
eitherEff eith ex = case eith of
  Left e -> throw ex e
  Right r -> pure r

-- | Inverse to 'runState'
stateEff :: (e1 <: es) => (s -> (a, s)) -> State s e1 -> Eff es a
stateEff f st = do
  s <- get st
  let (a, s') = f s
  put st s'
  pure a

-- | Inverse to 'yieldToList'
listEff :: (e1 <: es) => ([a], r) -> Stream a e1 -> Eff es r
listEff (as, r) y = do
  for_ as (yield y)
  pure r

test_localInHandler :: (e <: es) => SpecH e -> Eff es ()
test_localInHandler y = runReader "global" $ \re ->
  forEach
    (\y2 -> local re (const "local") (yield y2 ()))
    (\() -> assertEqual y "Reader local" "local" =<< ask re)

-- local run in one branch of a streamConsume should not affect ask in
-- the other branch.
test_streamConsumeReader :: (e <: es) => SpecH e -> Eff es ()
test_streamConsumeReader spech = do
  runReader @Int 0 $ \r -> do
    streamConsume
      ( \y -> do
          let s = yield y ()
          let check i = assertEqual spech "Reader local" i =<< ask r
          check 0
          s
          check 0
          s
          check 0
          local r (+ 1) $ do
            check 1
            s
            check 1
            local r (+ 1) $ do
              check 2
              s
              check 2
            check 1
            s
            check 1
          check 0
          s
          check 0
      )
      ( \a -> do
          let p = await a
          p
          local r (subtract 100) $ do
            p
            local r (subtract 100) $ do
              p
              local r (subtract 100) $ do
                p
                local r (subtract 100) $ do
                  forever p
      )

-- localHandle run in one branch of a streamConsume should not affect
-- askHandle in the other branch.
test_streamConsumeHandleReader :: (e <: es) => SpecH e -> Eff es ()
test_streamConsumeHandleReader spech = do
  runConstEffect @Int 0 $ \ce ->
    runHandleReader ce $ \r -> do
      streamConsume
        ( \y -> do
            let s = yield y ()
            let check i = do
                  MkConstEffect i' <- askHandle r
                  assertEqual spech "HandleReader local" i i'
            check 0
            s
            check 0
        )
        ( \a -> do
            let p = await a
            p
            localHandle r (\(MkConstEffect i) -> MkConstEffect (subtract 100 i)) $ do
              forever p
        )

-- Reader should interact well with withEffToIO_
test_unliftIOReader :: (e1 <: es, e2 <: es) => IOE e1 -> SpecH e2 -> Eff es ()
test_unliftIOReader io spech = do
  let foo :: (Int -> IO ()) -> (forall r. Int -> IO r -> IO r) -> IO ()
      foo myYield mySetLocal = do
        myYield 0
        mySetLocal 1 $ do
          myYield 1
          mySetLocal 2 $ do
            myYield 2

  runReader @Int 0 $ \r -> do
    withEffToIO_ io $ \effToIO -> do
      foo
        ( \i -> effToIO $ do
            i' <- ask r
            assertEqual spech "myLocal" i i'
        )
        (\x -> effToIO . local r (const x) . effIO io)
