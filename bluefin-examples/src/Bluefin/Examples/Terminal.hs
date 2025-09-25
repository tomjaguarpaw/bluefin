{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Bluefin.Examples.Terminal where

import Bluefin.Compound (Handle (mapHandle), useImplIn0)
import Bluefin.Eff (Eff, type (:&), type (:>))
import Bluefin.IO (IOE, effIO)

newtype Terminal e = MkTerminal (IOE e)

putStrLn :: (e :> es) => Terminal e -> String -> Eff es ()
putStrLn (MkTerminal ioe) = effIO ioe . Prelude.putStrLn

runTerminal ::
  forall termEff es r.
  (termEff :> es) =>
  IOE termEff ->
  (forall e. Terminal e -> Eff (e :& es) r) ->
  Eff es r
runTerminal ioe k = useImplIn0 $ k (MkTerminal (mapHandle ioe))
