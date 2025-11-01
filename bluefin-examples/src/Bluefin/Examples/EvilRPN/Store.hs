{-# LANGUAGE TemplateHaskell #-}

module Bluefin.Examples.EvilRPN.Store where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.Examples.EvilRPN.Language
import Bluefin.Exception
import Bluefin.IO
import Bluefin.State
import qualified Control.Exception as E
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Time
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Read (readMaybe)

data Store e = Store
  { pushOneImpl :: Int -> forall e'. Eff (e' :& e) [Int]
  , popTwoImpl :: forall e'. Eff (e' :& e) (Maybe (Int, Int))
  , devourInImpl :: UTCTime -> Int -> forall e'. Eff (e' :& e) ()
  , pulseImpl :: UTCTime -> forall e'. Eff (e' :& e) ()
  }

instance Handle Store where
  mapHandle c =
    Store
    { pushOneImpl = \a -> useImplUnder (pushOneImpl c a)
    , popTwoImpl = useImplUnder (popTwoImpl c)
    , devourInImpl = \a b -> useImplUnder (devourInImpl c a b)
    , pulseImpl = \a -> useImplUnder (pulseImpl c a)
    }

pushOne :: (e :> es) => Store e -> Int -> Eff es [Int]
pushOne e a = makeOp (pushOneImpl (mapHandle e) a)

popTwo :: (e :> es) => Store e -> Eff es (Maybe (Int, Int))
popTwo e = makeOp (popTwoImpl (mapHandle e))

devourIn :: (e :> es) => Store e -> UTCTime -> Int -> Eff es ()
devourIn e a b = makeOp (devourInImpl (mapHandle e) a b)

pulse :: (e :> es) => Store e -> UTCTime -> Eff es ()
pulse e a = makeOp (pulseImpl (mapHandle e) a)

data EvilStack = EvilStack
  { _stack :: [Int]
  , _hunger :: Set UTCTime
  }
  deriving (Show, Eq, Read)

makeLenses ''EvilStack

runStorePure
  :: forall e1 es r. (e1 :> es)
  => State EvilStack e1
  -> (forall e. Store e -> Eff (e :& es) r)
  -> Eff es r
runStorePure st k = do
  useImplIn
    k
    (Store
     { pushOneImpl = \i -> do
         modify st $ stack %~ (i:)
         view stack <$> get st
     , popTwoImpl = do
         (xs,rx) <- splitAt 2 . view stack <$> get st
         case xs of
           [a,b] -> do
             modify st $ stack .~ rx
             pure $ Just (a,b)
           _ -> pure Nothing
     , devourInImpl = \t d ->
         modify st $ hunger %~ Set.insert (addUTCTime (fromIntegral d) t)
     , pulseImpl = \t -> do
         s <- get st
         let (evil,r) = Set.spanAntitone (<= t) (s ^. hunger)
             l = Set.size evil
             (_,rs) = splitAt l (s ^. stack)
         put st $ EvilStack rs r
     }
    )

runStoreIO
  :: forall e1 e2 es r. (e1 :> es, e2 :> es)
  => FilePath
  -> IOE e1
  -> Exception HungerException e2
  -> (forall e. Store e -> Eff (e :& es) r)
  -> Eff es r
runStoreIO file io ex k = do
  let withFile :: (EvilStack -> Eff es (EvilStack, a)) -> Eff es a
      withFile f = do
        orig <- withMonadIO @_ @es io $ liftIO $ E.try $ readFile file
        case orig of
          Left err -> throw ex $ FileIOError err
          Right raw | Just s <- readMaybe raw -> do
            (new, x) <- f s
            res <- withMonadIO io $ liftIO $ E.try $ writeFile file $ show new
            case res of
              Left err -> throw ex $ FileIOError err
              _ -> pure x
          Right _ -> throw ex FileCorrupted

  useImplIn
    k
    (Store
     { pushOneImpl = \i -> useImpl $ withFile $ \es ->
         let es' = stack %~ (i:) $ es
         in pure (es', es' ^. stack)
     , popTwoImpl = useImpl $ withFile $ \es -> do
         let (xs, rs) = splitAt 2 $ es ^. stack
         pure $ case xs of
           [a,b] -> (stack .~ rs $ es, Just (a,b))
           _ -> (es, Nothing)
     , devourInImpl = \t d -> useImpl $ withFile $ \es -> do
         pure (hunger %~ Set.insert (addUTCTime (fromIntegral d) t) $ es, ())
     , pulseImpl = \t -> useImpl $ withFile $ \es -> do
         let (evil, r) = Set.spanAntitone (<= t) (es ^. hunger)
             l = Set.size evil
             (xs,rs) = splitAt l (es ^. stack)
         when (length xs /= l) $
           throw ex HungerException
         pure (EvilStack rs r, ())
     }
    )
