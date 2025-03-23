{-# LANGUAGE QualifiedDo #-}

module Bluefin.Internal.With where

import Data.Foldable (for_)
import Data.Traversable (for)
import Bluefin.Internal
import Bluefin.Internal.System.IO (hPutStr, withFile, IOMode (ReadMode))

-- This is `id` if you allow impredicative types
(>>=) ::
  ((forall e. f e -> Eff (e :& es) a) -> Eff es a) ->
  (forall e. f e -> Eff (e :& es) a) ->
  Eff es a
(>>=) handler = handler

f :: IO Integer
f = runEff $ \io -> do
  Bluefin.Internal.With.do
    h <- withFile io "file.txt" ReadMode
    s1 <- evalState 5
    s2 <- evalState 10
    do
      s <- get s1
      modify s2 (+ s)
      hPutStr h "hello world"
      get s2

{-
g :: Integer
g = runPureEff $ do
  Bluefin.Internal.With.do
    total <- evalState 0
    -- Don't think we can do this with QualifiedDo
    states <- for [1..10] $ \i -> do
      evalState i
    do
      for_ states $ \state -> do
        v <- get state
        modify total (+ v)
      get total
-}
