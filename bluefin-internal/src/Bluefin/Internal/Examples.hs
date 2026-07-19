module Bluefin.Internal.Examples where

import Bluefin.Internal
import Data.Proxy (Proxy (Proxy))

-- Fails to compile unless '(e <: es) => e <: (x :& es)' is incoherent
-- (otherwise I guess it "commits to it too soon")
example :: ()
example = runPureEff $
  evalModify () $ \st1 ->
    evalModify () $ \st2 -> do
      Proxy :: Proxy es <- effTag
      satisfied @(es <: es)

      Proxy :: Proxy e1 <- handleTag st1
      Proxy :: Proxy e2 <- handleTag st2

      satisfied @(e1 <: e1)
      satisfied @(e2 <: e2)
      satisfied @(e1 <: (e1 :& e2))
      satisfied @(e2 <: (e1 :& e2))

      satisfied @((e1 :& e2) <: (e1 :& e2))
