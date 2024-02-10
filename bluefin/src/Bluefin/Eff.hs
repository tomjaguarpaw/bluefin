module Bluefin.Eff
  ( -- | Bluefin's effects take place in the 'Eff' monad.  The most
    -- common effects are
    --
    --  * "Bluefin.State", for mutable state
    --  * "Bluefin.Exception", for exceptions
    --  * "Bluefin.Stream", for streams
    --  * "Bluefin.IO", for I/O

    -- * 'Eff' monad
    Eff,
    -- * Run an 'Eff'
    runEff,
    runEffIO,
    -- * Effect tracking
    Effects,
    (:>),
    (:&),
  )
where

import Bluefin.Internal
