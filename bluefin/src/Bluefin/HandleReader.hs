-- | 'HandleReader' is like t'Bluefin.Reader.Reader', generalized to
-- work for arbitrary t'Bluefin.Compound.Handle's.  'localHandle'
-- locally overrides the value of a handle in a well-scoped way.  The
-- original handle will be restored when you exit the @localHandle@
-- block regardless of whether the exit was normal or via an
-- exception.
--
-- @HandleReader@ supports functionality similiar to @effectful@'s
-- [@interpose@](https://hackage.haskell.org/package/effectful-core/docs/Effectful-Dispatch-Dynamic.html#v:interpose)
-- and @polysemy@'s
-- [@intercept@](https://hackage.haskell.org/package/polysemy/docs/Polysemy.html#v:intercept),
-- that is, locally augmenting an effect with new behaviors.

module Bluefin.HandleReader
  ( -- * Handle
    HandleReader,

    -- * Handlers
    runHandleReader,

    -- * Effectful operations
    askHandle,
    localHandle,
  )
where

import Bluefin.Internal
