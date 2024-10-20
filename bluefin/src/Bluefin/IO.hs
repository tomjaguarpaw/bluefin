module Bluefin.IO
  ( -- | You can run 'IO' operations inside 'Eff'.

    -- * Handle
    IOE,
    -- * Handlers
    runEff,
    -- * Effectful operations
    effIO,
    rethrowIO,
    -- * IO type classes
    withMonadIO,
    withEffToIO,
    -- ** @EffReader@
    EffReader,
    effReader,
    runEffReader,
  )
where

import Bluefin.Internal
