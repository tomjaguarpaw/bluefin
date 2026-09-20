-- | This is an old interface and will be deprecated in the
-- future. You are encouraged to use
-- "Bluefin.Capability.AskCapability" instead.
--
-- 'HandleReader' is like t'Bluefin.Reader.Reader', generalized to
-- work for arbitrary t'Bluefin.Compound.Handle's.  'localHandle'
-- locally overrides the value of a capability in a well-scoped way.  The
-- original capability will be restored when you exit the @localHandle@
-- block regardless of whether the exit was normal or via an
-- exception.
--
-- @HandleReader@ supports functionality similiar to @effectful@'s
-- [@interpose@](https://hackage.haskell.org/package/effectful-core/docs/Effectful-Dispatch-Dynamic.html#v:interpose)
-- and @polysemy@'s
-- [@intercept@](https://hackage.haskell.org/package/polysemy/docs/Polysemy.html#v:intercept),
-- that is, locally augmenting an effect with new behaviors.  If you
-- want to do the same in Bluefin you may want to start with
-- @Bluefin.GadtEffect.'Bluefin.GadtEffect.interpose`@.
module Bluefin.HandleReader
  ( -- * Handle
    HandleReader,

    -- * Handlers
    runHandleReader,

    -- * Effectful operations
    asksHandle,
    localHandle,

    -- ** Deprecated
    askHandle,
  )
where

import Bluefin.Internal
