-- | A safer interface to "System.IO.Handle"

module Bluefin.System.IO
  ( -- * Handle
    Handle,

    -- * Handlers
    withFile,

    -- * Effectful operations
    hPutChar,
    hPutStr,
    hPutStrLn,
    hFlush,

    -- * Unsafe

    unsafeWithHandle,
  )
where

import Bluefin.Internal.System.IO
