module Test.ThrowCatch where

import Bluefin.Internal
import Bluefin.Internal.Capability.ThrowCatch qualified as ThrowCatch
import Test.SpecH (SpecH, assertEqual)

test_throwCatch :: (e <: es) => SpecH e -> Eff es ()
test_throwCatch y = do
  assertEqual
    y
    "ThrowCatch.try catches a thrown exception"
    (Left @String @() "caught")
    (runPureEff (ThrowCatch.try $ \ex -> ThrowCatch.throw ex "caught"))
  assertEqual
    y
    "ThrowCatch.try returns a successful result"
    (Right @String @Int 42)
    (runPureEff (ThrowCatch.try $ \_ -> pure 42))
  assertEqual
    y
    "ThrowCatch.localTry catches a thrown exception"
    (Left @String @() "outer")
    ( runPureEff $ ThrowCatch.try $ \ex -> do
        localResult <- ThrowCatch.localTry ex (ThrowCatch.throw ex "inner")
        ThrowCatch.throw ex $ case localResult of
          Left "inner" -> "outer"
          Left _ -> "unexpected local exception"
          Right () -> "localTry did not catch"
    )
