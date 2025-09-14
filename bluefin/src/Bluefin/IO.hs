module Bluefin.IO
  ( -- | You can run 'IO' operations inside 'Eff'.

    -- * Handle
    IOE,

    -- * Handlers
    runEff_,
    runEff,

    -- * Effectful operations
    effIO,
    rethrowIO,

    -- * IO type classes
    withMonadIO,
    withEffToIO_,
    withEffToIOCloneHandle,

    -- ** @EffReader@
    EffReader,
    effReader,
    runEffReader,

    -- ** Deprecated versions
    withEffToIO,
  )
where

import Bluefin.Internal
import Bluefin.Internal.CloneableHandle
