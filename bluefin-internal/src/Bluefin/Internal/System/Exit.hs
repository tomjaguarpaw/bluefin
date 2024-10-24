module Bluefin.Internal.System.Exit
  ( module Bluefin.Internal.System.Exit,
    ExitCode,
  )
where

import Bluefin.Internal
  ( Eff,
    Handle (..),
    IOE,
    effIO,
    useImplIn,
    (:&),
    (:>),
  )
import System.Exit (ExitCode)
import qualified System.Exit as SE

newtype Exit e = UnsafeMkExit (IOE e)

instance Handle Exit where
  mapHandle (UnsafeMkExit io) = UnsafeMkExit (mapHandle io)

runExit ::
  (e1 :> es) =>
  IOE e1 ->
  (forall e. Exit e -> Eff (e :& es) r) ->
  -- | ͘
  Eff es r
runExit io k = useImplIn k (mapHandle (UnsafeMkExit io))

unsafeWrapExit :: Exit e -> IO a -> Eff (e :& es) a
unsafeWrapExit (UnsafeMkExit io) = effIO io

exitWith :: Exit e -> ExitCode -> Eff (e :& es) a
exitWith exit ec = unsafeWrapExit exit (SE.exitWith ec)

exitFailure :: Exit e -> Eff (e :& es) a
exitFailure ec = unsafeWrapExit ec SE.exitFailure

exitSuccess :: Exit e -> Eff (e :& es) a
exitSuccess ec = unsafeWrapExit ec SE.exitSuccess

die :: Exit e -> String -> Eff (e :& es) a
die ec = unsafeWrapExit ec . SE.die
