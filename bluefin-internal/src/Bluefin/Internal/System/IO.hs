module Bluefin.Internal.System.IO
  ( module Bluefin.Internal.System.IO,
    System.IO.IOMode (..),
  )
where

import Bluefin.Internal
  ( Eff,
    IOE,
    bracket,
    effIO,
    mapHandle,
    useImplIn,
    (:&),
    (:>),
  )
import Bluefin.Internal qualified
import System.IO qualified

-- We can probably get away without the IOE and just use
-- unsafeProvideIO on all Handle functions
data Handle e = UnsafeMkHandle System.IO.Handle (IOE e)

instance Bluefin.Internal.Handle Handle where
  mapHandle (UnsafeMkHandle h io) =
    UnsafeMkHandle h (mapHandle io)

withFile ::
  (e1 :> es) =>
  IOE e1 ->
  FilePath ->
  System.IO.IOMode ->
  (forall e. Handle e -> Eff (e :& es) r) ->
  -- | ͘
  Eff es r
withFile io fp iomode k =
  bracket
    ( effIO io (System.IO.openFile fp iomode)
    )
    ( \handle -> effIO io (System.IO.hClose handle)
    )
    ( \handle -> useImplIn k (UnsafeMkHandle handle (mapHandle io))
    )

hPutChar ::
  (e :> es) =>
  Handle e ->
  Char ->
  -- | ͘
  Eff es ()
hPutChar h = unsafeWithHandle h . flip System.IO.hPutChar

hPutStr ::
  (e :> es) =>
  Handle e ->
  String ->
  -- | ͘
  Eff es ()
hPutStr h = unsafeWithHandle h . flip System.IO.hPutStr

hPutStrLn ::
  (e :> es) =>
  Handle e ->
  String ->
  -- | ͘
  Eff es ()
hPutStrLn h = unsafeWithHandle h . flip System.IO.hPutStrLn

hFlush ::
  (e :> es) =>
  Handle e ->
  -- | ͘
  Eff es ()
hFlush h = unsafeWithHandle h System.IO.hFlush

hGetLine ::
  (e :> es) =>
  Handle e ->
  -- | ͘
  Eff es String
hGetLine h = unsafeWithHandle h System.IO.hGetLine

hIsEOF ::
  (e :> es) =>
  Handle e ->
  -- | ͘
  Eff es Bool
hIsEOF h = unsafeWithHandle h System.IO.hIsEOF

-- | If there's a "System.IO.Handle"-using function you need that
-- isn't included here then you can [open an
-- issue](https://github.com/tomjaguarpaw/bluefin/issues/new) to
-- request it be added.  In the meantime you can define it yourself with @unsafeWithHandle@.
unsafeWithHandle ::
  (e1 :> es) =>
  Handle e1 ->
  (System.IO.Handle -> IO r) ->
  Eff es r
unsafeWithHandle (UnsafeMkHandle h io) k = effIO io (k h)
