-- | Like "Bluefin.DslBuilder", but when you want to be able to run
-- additional effects as well.

module Bluefin.DslBuilderEff (
    DslBuilderEff,
    dslBuilderEff,
    runDslBuilderEff,
  )
where

import Bluefin.Internal.DslBuilderEff
