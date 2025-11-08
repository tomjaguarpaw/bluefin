{-# LANGUAGE TemplateHaskell #-}

module Bluefin.Examples.EvilRPN.Application where

import Bluefin.Contrib.Application
import Bluefin.Coroutine
import Bluefin.Eff
import Bluefin.Examples.EvilRPN.Language
import Bluefin.Examples.EvilRPN.Store
import Bluefin.State
import Bluefin.Stream
import Control.Lens
import Control.Monad
import Data.Function
import Data.Time

data ApplicationState = ApplicationState
  { _operationCount :: Int
  , _stackInsertCount :: Int
  }
  deriving (Show, Eq)

makeLenses ''ApplicationState

mainLoop
  :: (e1 :> es)
  => ApplicationState
  -> (UTCTime, Maybe RPN)
  -> Application RPN Output Store e1
  -> Eff es ApplicationState
mainLoop initialState firstInput app =
  fmap snd $ runState initialState $ \appState ->
  flip fix firstInput $ \loop (t,ev) ->
  (maybe (pure ()) loop <=< yieldCoroutine (emitter app) . fst) =<< yieldToList
  (\y -> case ev of
      Nothing ->
        pulse (store app) t
      Just cmd -> case cmd of
        Evil i -> devourIn (store app) t i
        Push i -> do
          modify appState $ stackInsertCount %~ succ
          pushOne (store app) i >>= yield y . Stack
        -- Operations
        _ -> do
          modify appState $ operationCount %~ succ
          popTwo (store app) >>= maybe (yield y InsufficientStack)
            (yield y . Stack <=< pushOne (store app) . uncurry
             (case cmd of
                Plus -> (+)
                Minus -> (-)
                Multiply -> (*)
             )
            )
