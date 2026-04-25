-- | 'AskCapability' is like t'Bluefin.Capability.Ask.Ask', generalized to
-- work for arbitrary t'Bluefin.Compound.Handle's.  'localCapability'
-- locally overrides the value of a capability in a well-scoped way.  The
-- original capability will be restored when you exit the @localCapability@
-- block regardless of whether the exit was normal or via an
-- exception.
--
-- @AskCapability@ supports functionality similiar to @effectful@'s
-- [@interpose@](https://hackage.haskell.org/package/effectful-core/docs/Effectful-Dispatch-Dynamic.html#v:interpose)
-- and @polysemy@'s
-- [@intercept@](https://hackage.haskell.org/package/polysemy/docs/Polysemy.html#v:intercept),
-- that is, locally augmenting an effect with new behaviors.  If you
-- want to do the same in Bluefin you may want to start with
-- @Bluefin.GadtEffect.'Bluefin.GadtEffect.interpose`@.
module Bluefin.Capability.AskCapability
  ( -- * Handle
    AskCapability,

    -- * Handlers
    runAskCapability,

    -- * Effectful operations
    askCapability,
    asksCapability,
    localCapability,
  )
where

import Bluefin.Internal
