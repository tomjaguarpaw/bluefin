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
    oneWayCoercibleTrustMe,
    useImpl,
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

-- | A dynamic terminal capability. Its implementation is supplied as a
-- product of effectful operations rather than fixed by the capability type.
data DynamicTerminal e = MkDynamicTerminal
  { dynamicGetLineImpl :: Eff e String,
    dynamicPutStrLnImpl :: String -> Eff e ()
  }
  deriving (Handle) via OneWayCoercibleHandle DynamicTerminal

instance
  (e <: es) =>
  OneWayCoercible (DynamicTerminal e) (DynamicTerminal es)
  where
  oneWayCoercibleImpl = oneWayCoercibleTrustMe $ \terminal ->
    MkDynamicTerminal
      { dynamicGetLineImpl = useImpl (dynamicGetLineImpl terminal),
        dynamicPutStrLnImpl = useImpl . dynamicPutStrLnImpl terminal
      }

dynamicGetLine :: (e <: es) => DynamicTerminal e -> Eff es String
dynamicGetLine terminal = dynamicGetLineImpl (mapHandle terminal)

dynamicPutStrLn :: (e <: es) => DynamicTerminal e -> String -> Eff es ()
dynamicPutStrLn terminal = dynamicPutStrLnImpl (mapHandle terminal)

runDynamicTerminal ::
  forall e1 es r.
  (e1 <: es) =>
  IOE e1 ->
  (forall e. DynamicTerminal e -> Eff (e :& es) r) ->
  Eff es r
runDynamicTerminal ioe k =
  useImplIn
    k
    MkDynamicTerminal
      { dynamicGetLineImpl = effIO ioe Prelude.getLine,
        dynamicPutStrLnImpl = effIO ioe . Prelude.putStrLn
      }

dynamicTerminalExample :: IO ()
dynamicTerminalExample = runEff $ \io -> runDynamicTerminal io dynamicTerminalAction

dynamicTerminalAction :: (e <: es) => DynamicTerminal e -> Eff es ()
dynamicTerminalAction terminal = do
  dynamicPutStrLn terminal "What is your name?"
  name <- dynamicGetLine terminal
  dynamicPutStrLn terminal ("Hello, " <> name)
