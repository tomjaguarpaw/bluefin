{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Bluefin.Internal.Exception where

import Bluefin.Internal hiding (UnsafeMkEff, b)
import Bluefin.Internal.CloneableHandle (app, (:~>))
import Bluefin.Internal.Exception.Scoped (InFlight)
import Bluefin.Internal.Exception.Scoped qualified as SE
import Bluefin.Internal.OneWayCoercible
  ( Generic,
    OneWayCoercible (oneWayCoercibleImpl),
    gOneWayCoercible,
    oneWayCoerce,
    unsafeOneWayCoercible,
  )
import Control.Exception qualified as CE
import Data.Proxy (Proxy)

data HandledKey ret = forall ex. MkHandledKey !(SE.Exception ex) (ex -> ret)

checkHandledKey :: HandledKey ret -> InFlight -> Maybe ret
checkHandledKey (MkHandledKey k handler) inflight =
  handler <$> SE.checkException k inflight

instance Functor HandledKey where
  fmap f (MkHandledKey ex r) = MkHandledKey ex (fmap f r)

runBodyWithHandlers ::
  ( [HandledKey (resource -> Eff es a)],
    BracketBase resource b resource a es
  ) ->
  Eff es a
runBodyWithHandlers (handledKeys, bb) =
  unsafeProvideIO $ \io -> withEffToIO_ io $ \runInIO -> do
    let MkBracketBase {..} = useImplBracketBase bb
    CE.mask $ \unmasked -> do
      resource <- runInIO acquire
      eBodyRes <-
        (CE.try . unmasked . runInIO . body) resource
      runInIO $ case eBodyRes of
        Right bodyRes -> normalRelease resource bodyRes
        Left e -> case CE.fromException e >>= findHandler handledKeys of
          Nothing -> do
            unknownExceptionRelease resource
            effIO io (CE.throwIO e)
          Just handler -> useImpl (handler resource)
  where
    firstJust _ [] = Nothing
    firstJust f (x : xs) = case f x of
      Nothing -> firstJust f xs
      Just r -> Just r

    findHandler :: [HandledKey a] -> SE.InFlight -> Maybe a
    findHandler hks inflight = firstJust (flip checkHandledKey inflight) hks

newtype MakeExceptions resource a h es
  = MkMakeExceptions (Eff es (HandlerUnwrapped resource a h es))

data HandlerUnwrapped resource a h es
  = MkHandlerUnwrapped
      [HandledKey (resource -> Eff es a)]
      (forall r. (forall e. h e -> Eff (e :& es) r) -> Eff es r)

instance (Handle h) => Handle (MakeExceptions resource a h) where
  handleImpl = handleOneWayCoercible

instance (Handle h) => Handle (HandlerUnwrapped resource a h) where
  handleImpl = handleOneWayCoercible

instance
  (Handle h, e :> es) =>
  OneWayCoercible (MakeExceptions resource a h e) (MakeExceptions resource a h es)
  where
  oneWayCoercibleImpl = unsafeOneWayCoercible

instance
  (Handle h, e :> es) =>
  OneWayCoercible (HandlerUnwrapped resource a h e) (HandlerUnwrapped resource a h es)
  where
  oneWayCoercibleImpl = unsafeOneWayCoercible

pureHandlerUnwrapped :: h e -> HandlerUnwrapped resource a h e
pureHandlerUnwrapped h = MkHandlerUnwrapped [] (\k -> makeOp (k h))

pureMakeExceptions ::
  h e ->
  -- | ͘
  MakeExceptions r a h e
pureMakeExceptions h = MkMakeExceptions (pure (pureHandlerUnwrapped h))

apHandlerUnwrapped ::
  (Handle h1, Handle h2) =>
  HandlerUnwrapped r a (h1 :~> h2) e ->
  HandlerUnwrapped r a h1 e ->
  HandlerUnwrapped r a h2 e
apHandlerUnwrapped
  hrh1h2@(MkHandlerUnwrapped l1 _)
  hrh1@(MkHandlerUnwrapped l2 _) =
    MkHandlerUnwrapped
      (l1 <> l2)
      ( \k -> case mapHandle hrh1h2 of
          MkHandlerUnwrapped _ f -> f $ \h1h2 ->
            case mapHandle hrh1 of
              MkHandlerUnwrapped _ x -> x $ \h1 ->
                useImplIn k (app (mapHandle h1h2) (mapHandle h1))
      )

apMakeExceptions ::
  (Handle h1, Handle h2) =>
  MakeExceptions r a (h1 :~> h2) e ->
  MakeExceptions r a h1 e ->
  -- | ͘
  MakeExceptions r a h2 e
apMakeExceptions (MkMakeExceptions mh1h2) (MkMakeExceptions mh1) = MkMakeExceptions $ do
  h1h2 <- mh1h2
  h1 <- mh1
  pure (apHandlerUnwrapped h1h2 h1)

fmapHandlerUnwrapped ::
  (Handle h1, Handle h2) =>
  (h1 :~> h2) e ->
  HandlerUnwrapped resource a h1 e ->
  HandlerUnwrapped resource a h2 e
fmapHandlerUnwrapped f h = pureHandlerUnwrapped f `apHandlerUnwrapped` h

fmapMakeExceptions ::
  (Handle h1, Handle h2) =>
  (h1 :~> h2) e ->
  MakeExceptions r a h1 e ->
  -- | ͘
  MakeExceptions r a h2 e
fmapMakeExceptions f (MkMakeExceptions mh1) = MkMakeExceptions $ do
  h1 <- mh1
  pure (fmapHandlerUnwrapped f h1)

catchWithResource ::
  forall ex r a es.
  (r -> ex -> Eff es a) ->
  -- | ͘
  MakeExceptions r a (Exception ex) es
catchWithResource f = MkMakeExceptions $ unsafeProvideIO $ \io -> do
  scopedEx <- effIO io (SE.newException @ex)
  let hk = MkHandledKey scopedEx (flip f)
  pure
    ( MkHandlerUnwrapped
        [hk]
        ( \k -> do
            let ex = MkException (\e -> unsafeProvideIO $ \io' -> effIO io' (SE.throw scopedEx e))
            makeOp (k ex)
        )
    )

-- | A generalization of 'bracket' that enables distinguishing
-- exceptional from normal exit.
--
-- [@r@]: The type of the allocated resource
-- [@b@]: The return type of the body

-- Alsa
-- https://www.stackage.org/haddock/lts-24.26/exceptions-0.10.9/Control-Monad-Catch.html#v:generalBracket
generalBracket ::
  forall r b h a es.
  (Handle h) =>
  -- | Acquire the resource
  Eff es r ->
  -- | Construct the handle @h@ to pass into the body
  MakeExceptions r a h es ->
  -- | To run on normal termination
  (r -> b -> Eff es a) ->
  -- | To run on unknown exception
  (r -> Eff es ()) ->
  -- | Body
  (forall e. h e -> r -> Eff (e :& es) b) ->
  Eff es a
generalBracket
  acquire'
  (MkMakeExceptions handlers)
  normalRelease'
  unknownExceptionRelease'
  body' =
    do
      MkHandlerUnwrapped handledKeys k <- handlers
      k $ \h -> do
        _ :: Proxy e1 <- handleTag h
        let bb :: BracketBase r b r a (e1 :& es)
            bb =
              MkBracketBase
                { acquire = useImpl acquire',
                  normalRelease = \r b -> useImpl (normalRelease' r b),
                  unknownExceptionRelease =
                    \r -> useImpl (unknownExceptionRelease' r),
                  body = \resource -> useImpl (body' h resource)
                }
        runBodyWithHandlers ((fmap . fmap . fmap) useImpl handledKeys, bb)

data BracketBase resource bodyRes r a es = MkBracketBase
  { -- | Acquire the resource
    --
    -- This is run inside an asynchronous exception 'CE.mask'.
    acquire :: !(Eff es resource),
    -- | Release the resource after normal exit
    --
    -- This is run inside an asynchronous exception 'CE.mask'.
    normalRelease :: !(resource -> bodyRes -> Eff es a),
    -- | Release the resource after exit due to an unknown exception.
    --
    -- The exception will continue to be raised after this.
    --
    -- This is run inside an asynchronous exception 'CE.mask'.
    unknownExceptionRelease :: !(resource -> Eff es ()),
    -- | Use the resource
    body :: !(r -> Eff es bodyRes)
  }
  deriving (Generic)

instance
  (e :> es) =>
  OneWayCoercible
    (BracketBase resource bodyRes r a e)
    (BracketBase resource bodyRes r a es)
  where
  oneWayCoercibleImpl = gOneWayCoercible

useImplBracketBase ::
  (e :> es) =>
  BracketBase resource b r a e ->
  BracketBase resource b r a es
useImplBracketBase = oneWayCoerce
