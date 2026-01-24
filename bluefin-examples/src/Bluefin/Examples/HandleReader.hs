module Bluefin.Examples.HandleReader where

import Bluefin.Compound (Handle, mapHandle, useImpl)
import Bluefin.Eff (Eff, runEff, (:&), (:>))
import Bluefin.HandleReader
  ( HandleReader,
    askHandle,
    localHandle,
    runHandleReader,
  )
import Bluefin.IO (effIO)
import Bluefin.State (State, evalState, get, modify)
import Bluefin.Stream (Stream, forEach, yield)

-- | @SummableStream@ is a @Stream@ that can be locally overridden
newtype SummableStream e = MkSummableStream (HandleReader (Stream Int) e)

-- | Run the @SummableStream@ in a @Stream@
runSummableStream ::
  (e1 :> es) =>
  Stream Int e1 ->
  (forall e. SummableStream e -> Eff (e :& es) r) ->
  Eff es r
runSummableStream y k =
  runHandleReader y $ \hr -> do
    k (MkSummableStream hr)

-- | Yield to the @SummableStream@
yieldSummable ::
  (e1 :> es) =>
  SummableStream e1 ->
  Int ->
  Eff es ()
yieldSummable (MkSummableStream hr) n = onHandle hr (\y -> yield y n)

onHandle ::
  (Handle h, e1 :> es) =>
  HandleReader h e1 ->
  (forall e. h e -> Eff e r) ->
  Eff es r
onHandle hr k = do
  h <- askHandle hr
  k h

-- | Locally override the @SummableStream@ so that @yieldSummable@, as
-- well as yielding to the @Stream@ as normal, also accumulates into
-- the @State@.
sumYields ::
  (e1 :> es, e2 :> es) =>
  SummableStream e1 ->
  State Int e2 ->
  Eff es r ->
  Eff es r
sumYields (MkSummableStream hr) st body = do
  yorig <- askHandle hr
  -- In the body, hr is modified so that it both modifies the State
  -- and yields to the original Stream
  forEach
    ( \ynested -> do
        localHandle
          hr
          ( \_ -> mapHandle ynested
          )
          (useImpl body)
    )
    ( \i -> do
        -- yield to the original Stream
        yield yorig i
        -- modify the State
        modify st (+ i)
    )

-- ghci> exampleHandleReader
-- 1
-- 2
-- 3
-- 4
-- Total from summed block was 7
exampleHandleReader :: IO ()
exampleHandleReader = runEff $ \io -> do
  evalState 0 $ \st -> do
    forEach
      ( \y -> runSummableStream y $ \sb -> do
          yieldSummable sb 1
          yieldSummable sb 2
          sumYields sb st $ do
            -- The yields in this body will be accumulated into st
            yieldSummable sb 3
            yieldSummable sb 4
      )
      (effIO io . print @Int)

    total <- get st
    let msg = "Total from summed block was " <> show total
    effIO io (putStrLn msg)
