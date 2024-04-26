-- | = Dynamic exceptions
--
-- This is the vanilla exception mechanism from @IO@.
-- Use this module to handle exceptions from external (non-bluefin) APIs.
--
-- This module also enables a style of resource management where clean up happens
-- in exception handlers (e.g., using 'bracket', 'finally'). This is also compatible
-- with algebraic effect handlers using cancellable continuations
-- ("Bluefin.Algae.Cancellable").

module Bluefin.Exception.Dynamic
  ( DynExn
  , ioeToDynExn
  , throw
  , catch
  , bracket
  , finally
  , onException
  , throwIO
  , catchIO
  ) where

import Bluefin.Internal.Exception.Dynamic
