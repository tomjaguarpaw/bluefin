module Bluefin.Exception.GeneralBracket
  ( -- * Effectful functions
    generalBracket,

    -- * Handle
    MakeExceptions,
    catchWithResource,
    pureMakeExceptions,
    apMakeExceptions,
    fmapMakeExceptions,

    -- * @:~>@
    (:~>),
    abstract,
  )
where

import Bluefin.Internal.CloneableHandle
import Bluefin.Internal.Exception
