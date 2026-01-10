{-# LANGUAGE DerivingVia #-}

module Test.GeneralBracket where

import Bluefin.Internal
import Bluefin.Internal.CloneableHandle (abstract)
import Bluefin.Internal.Exception
import Bluefin.Internal.OneWayCoercible
import Control.Exception (ErrorCall)
import Test.SpecH (SpecH, assertEqual)

test_generalBracket :: (e1 :> es, e2 :> es) => IOE e1 -> SpecH e2 -> Eff es ()
test_generalBracket io s = do
  (actual, ()) <- yieldToList $ \y -> example io y

  assertEqual s "generalBracket" actual exampleResult

data ThreeExceptions e
  = MkThreeExceptions
      !(Exception Int e)
      !(Exception Bool e)
      !(Exception Char e)
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle ThreeExceptions

instance (e :> es) => OneWayCoercible (ThreeExceptions e) (ThreeExceptions es) where
  oneWayCoercibleImpl = gOneWayCoercible

threeExceptionsMakeExceptions ::
  forall es e1.
  (e1 :> es) =>
  Stream String e1 ->
  MakeExceptions String () ThreeExceptions es
threeExceptionsMakeExceptions y =
  ( abstract $ \e1 ->
      abstract $ \e2 ->
        abstract $ \e3 ->
          MkThreeExceptions (mapHandle e1) (mapHandle e2) (mapHandle e3)
  )
    `fmapMakeExceptions` catchAndReleasePrint
    `apMakeExceptions` catchAndReleasePrint
    `apMakeExceptions` catchAndReleasePrint
  where
    catchAndReleasePrint ::
      (Show ex) => MakeExceptions String () (Exception ex) es
    catchAndReleasePrint =
      catchWithResource (\i s -> yield y (show (i, s)))

example ::
  forall es e1 e2.
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Stream String e2 ->
  Eff es ()
example io y = do
  let s = yield y

  s "Normal termination:"
  g (\_ _ -> pure ())
  s ""
  s "Known exception termination:"
  g (\(MkThreeExceptions e1 _ _) _ -> throw e1 5)
  s ""
  s "Unknown exception termination:"
  catch
    ( \ex -> rethrowIO @ErrorCall io ex $ do
        g (\_ _ -> undefined)
    )
    (\_ -> s "Caught ErrorCall")
  where
    -- If you want to avoid writing this signature use
    --  NoMonoLocalBinds and NoMonomorphismRestriction
    g ::
      (e2 :> es') =>
      ( forall e. ThreeExceptions e -> String -> Eff (e :& es') r
      ) ->
      Eff es' ()
    g =
      generalBracket
        (pure "Hello")
        (threeExceptionsMakeExceptions y)
        (\_ _ -> yield y "Terminating normally")
        (\_ -> yield y "Terminating with unknownn exception")

exampleResult :: [String]
exampleResult = fst $ runPureEff $ yieldToList $ \y -> do
  let e = yield y
  e "Normal termination:"
  e "Terminating normally"
  e ""
  e "Known exception termination:"
  e "(\"Hello\",5)"
  e ""
  e "Unknown exception termination:"
  e "Terminating with unknownn exception"
  e "Caught ErrorCall"
