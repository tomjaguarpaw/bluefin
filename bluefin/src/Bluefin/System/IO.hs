-- | A safer interface to @System.IO.'System.IO.Handle'@

module Bluefin.System.IO
  ( -- * Handle
    Handle,

    -- * Handlers
    withFile,

    -- * Effectful operations
    hPutChar,
    hPutStr,
    hPutStrLn,
    hGetLine,
    hIsEOF,
    hFlush,

    -- * Unsafe

    unsafeWithHandle,
  )
where

import Bluefin.Internal.System.IO
