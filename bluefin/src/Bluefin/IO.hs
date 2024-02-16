module Bluefin.IO
  ( -- | You can run 'IO' operations inside 'Eff'.

    -- * Handle
    IOE,
    -- * Handlers
    runEffIO,
    -- * Effectful operations
    effIO,
    -- * IO type classes
    withMonadIO,
    EffReader (MkEffReader),
  )
where

import Bluefin.Internal
