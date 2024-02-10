module Bluefin.IO
  ( -- | You can run 'IO' operations inside 'Eff'.

    -- * Handle
    IOE,
    -- * Handlers
    runEffIO,
    -- * Effectful operations
    effIO,
  )
where

import Bluefin.Internal
