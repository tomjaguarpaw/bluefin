{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Bluefin.Examples.Terminal where

import Bluefin.Compound
  ( Handle,
    OneWayCoercible (oneWayCoercibleImpl),
    OneWayCoercibleHandle (..),
    mapHandle,
    oneWayCoercibleNewtypeHandle,
    useImplIn,
  )
import Bluefin.Eff (Eff, runEff, type (:&), type (<:))
import Bluefin.IO (IOE, effIO)
import Prelude hiding (getLine, putStrLn)
import Prelude qualified

newtype Terminal e = MkTerminal (IOE e)
  deriving (Handle) via OneWayCoercibleHandle Terminal

instance (e <: es) => OneWayCoercible (Terminal e) (Terminal es) where
  oneWayCoercibleImpl = oneWayCoercibleNewtypeHandle @IOE

getLine :: (e <: es) => Terminal e -> Eff es String
getLine (MkTerminal ioe) = effIO ioe Prelude.getLine

putStrLn :: (e <: es) => Terminal e -> String -> Eff es ()
putStrLn (MkTerminal ioe) = effIO ioe . Prelude.putStrLn

runTerminal ::
  forall termEff es r.
  (termEff <: es) =>
  IOE termEff ->
  (forall e. Terminal e -> Eff (e :& es) r) ->
  Eff es r
runTerminal ioe k = useImplIn k (MkTerminal (mapHandle ioe))

staticTerminalExample :: IO ()
staticTerminalExample = runEff $ \io -> runTerminal io staticTerminalAction

staticTerminalAction :: (e <: es) => Terminal e -> Eff es ()
staticTerminalAction terminal = do
  putStrLn terminal "What is your name?"
  name <- getLine terminal
  putStrLn terminal ("Hello, " <> name)
