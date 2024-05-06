-- | = Custom handles
--
-- Create Bluefin effects with your own types of handles.

module Bluefin.Handle
  ( -- * Effect signatures
    Sig
  , CovariantSig(..)

    -- * Handle types
  , HandleImpl
  , Handle

    -- * Handle creation
  , with
  ) where

import Bluefin.Internal.Handle
