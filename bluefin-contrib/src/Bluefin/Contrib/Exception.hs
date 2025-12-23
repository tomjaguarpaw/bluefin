{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Bluefin.Contrib.Exception
  ( generalBracket,
    Bracket,
    Bracket'(..),
    BracketBase,
    BracketBase'(..),
    BracketCatch(..),
  )
where

import Bluefin.Internal
import Bluefin.Internal.Key
import Bluefin.Internal.Exception.Scoped qualified as SE
import Control.Applicative
import Control.Exception qualified as CE
import Data.Coerce

data HandledKey ret = forall ex. MkHandledKey !(Key ex) (ex -> ret)

-- | A generalization of 'bracket' that enables distinguishing exceptional from normal exit.
--
-- [@resource@]: The type of resource to allocate
-- [@bodyRes@]: The type of value produced by using the resource
--
-- See 'Bracket'.
generalBracket :: Bracket es resource bodyRes a -> Eff es a
generalBracket bracketArgs = UnsafeMkEff $ do
    let
      mkExceptions ::
        [HandledKey (resource -> IO a)] ->
        forall bodyEs. Bracket' es bodyEs resource bodyRes a ->
        IO ([HandledKey (resource -> IO a)], BracketBase' IO IO resource bodyRes a)
      mkExceptions acc (Base base) = pure (acc, coerce base)
      mkExceptions acc (Catch @_ @_ @_ @_ @_ @ex (MkBracketCatch {..})) = do
        k <- newKey @ex
        let
          hk = MkHandledKey k (coerce catchAndRelease)
          scopedEx = SE.MkException k
        mkExceptions (hk : acc) (body (MkException (UnsafeMkEff . SE.throw scopedEx)))
    (handledKeys, (MkBracketBase {..})) <- mkExceptions [] bracketArgs
    CE.mask $ \unmasked -> do
      resource <- acquire
      eBodyRes <- tryWithContext . unmasked $ body resource
      case eBodyRes of
        Right bodyRes -> normalRelease resource bodyRes
        Left e -> case fromException e >>= findHandler handledKeys of
          Nothing -> do
            unknownExceptionRelease resource
            rethrowIO_ e
          Just handler -> handler resource
  where
    findHandler [] _ = Nothing
    findHandler ((MkHandledKey k handler) : tl) inflight
      =  handler <$> SE.check k inflight
     <|> findHandler tl inflight

    -- Properly handle exception context on newer GHCs.
#if MIN_VERSION_base(4,21,0)
    tryWithContext = CE.tryWithContext
    rethrowIO_ = CE.rethrowIO
    fromException (CE.ExceptionWithContext _ e) = CE.fromException e
#else
    tryWithContext = CE.try
    rethrowIO_ = CE.throwIO
    fromException = CE.fromException
#endif

-- | A specification for how to acquire, use, and release a resource
--
-- See 'Bracket''
type Bracket es = Bracket' es es

-- | A specification for how to acquire, use, and release a resource
--
-- [@resource@]: The type of resource to allocate
-- [@bodyRes@]: The type of value produced by using the resource
data Bracket' releaseEs bodyEs resource bodyRes a
  = -- | Specify how to handle a known exception (see 'BracketCatch')
    forall ex. Catch !(BracketCatch releaseEs bodyEs resource bodyRes a ex)
  | -- | Specify how to manage the resource when no known exception is thrown (see 'BracketBase')
    Base !(BracketBase releaseEs bodyEs resource bodyRes a)

-- | Specify how to handle a known exception
--
-- [@resource@]: The type of resource to allocate
-- [@bodyRes@]: The type of value produced by using the resource
-- [@ex@]: The exception type
data BracketCatch releaseEs bodyEs resource bodyRes a ex = MkBracketCatch
  { -- | Catch the exception and release the resource
    --
    -- Depending on your use case, you may want to
    -- re-raise the exception instead of producing
    -- the needed results some other way. You can
    -- do so by enclosing this 'generalBracket' call
    -- in a context that has access to a t'Bluefin.Exception.Exception'
    -- capability of the same type.
    --
    -- This is run inside an asynchronous exception 'CE.mask'.
    catchAndRelease :: !(ex -> resource -> Eff releaseEs a)
  , -- | Specify the rest of how to acquire, use, and release the resource,
    -- in a context where the known exception can be thrown
    body :: !(forall e. Exception ex e -> Bracket' releaseEs (e :& (bodyEs)) resource bodyRes a)
  }

-- | Specify how to manage the resource assuming no known exceptions are thrown
--
-- See 'BracketBase''
type BracketBase releaseEs bodyEs = BracketBase' (Eff releaseEs) (Eff bodyEs)

-- | Specify how to manage the resource assuming no known exceptions are thrown
--
-- [@resource@]: The type of resource to allocate
-- [@bodyRes@]: The type of value produced by using the resource
data BracketBase' releaseM bodyM resource bodyRes a = MkBracketBase
  { -- | Acquire the resource
    --
    -- This is run inside an asynchronous exception 'CE.mask'.
    acquire :: !(releaseM resource)
  , -- | Release the resource after normal exit
    --
    -- This is run inside an asynchronous exception 'CE.mask'.
    normalRelease :: !(resource -> bodyRes -> releaseM a)
  , -- | Release the resource after exit due to an unknown exception.
    --
    -- The exception will continue to be raised after this.
    --
    -- This is run inside an asynchronous exception 'CE.mask'.
    unknownExceptionRelease :: !(resource -> releaseM ())
  , -- | Use the resource
    body :: !(resource -> bodyM bodyRes)
  }
