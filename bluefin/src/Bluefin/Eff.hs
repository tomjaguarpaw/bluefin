module Bluefin.Eff
  ( -- * 'Eff' monad
    Eff,

    -- * Run an 'Eff'
    runPureEff,
    runEff,

    -- * Resource management
    bracket,
    finally,

    -- * Type classes

    -- | See "Bluefin.Eff.IO" for the most direct way of doing I/O in
    -- Bluefin.  If you really want to use 'MonadIO' you can use
    -- 'withMonadIO'.
    withMonadIO,
    withMonadFail,

    -- * Effect tracking
    Effects,
    (:>),
    (:&),

    -- * Deprecated
    runEff_,
  )
where

import Bluefin.Internal
