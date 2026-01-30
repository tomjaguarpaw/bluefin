module Bluefin.Examples.ST where

import Bluefin.Eff
import Bluefin.IO (effIO)
import Bluefin.Internal.ST
import Control.Monad.ST (ST)
import Data.HashTable.ST.Basic qualified as HT
import Data.STRef qualified as STRef

-- ghci> example
-- 0
-- 10
-- <HashTable>
-- [("hello",42),("bye",1099)]
-- No more ST
example :: IO ()
example = runEff_ $ \io -> do
  do
    runST $ \st -> do
      ref <- effST st (STRef.newSTRef @Int 0)
      v1 <- effST st (STRef.readSTRef ref)
      effIO io (print v1)
      effST st (STRef.modifySTRef ref (+ 10))
      v2 <- effST st (STRef.readSTRef ref)
      effIO io (print v2)

      ht <- effST st (HT.new @_ @String @Int)

      effST st (HT.insert ht "hello" 42)
      effST st (HT.insert ht "bye" 99)

      effIO io (print ht)

      effST st (HT.mutate ht "bye" $ \m -> (fmap (+ 1000) m, ()))

      htList <- effST st (toList ht)
      effIO io (print htList)

  effIO io (putStrLn "No more ST")

toList :: HT.HashTable s k v -> ST s [(k, v)]
toList ht = do
  HT.foldM (\a kv -> pure (kv : a)) [] ht
