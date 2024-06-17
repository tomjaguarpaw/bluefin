{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Bluefin.Examples.DB where

import Bluefin.Compound (useImpl, useImplIn)
import Bluefin.Eff (Eff, (:&), (:>))
import qualified Bluefin.Eff as BF
import Bluefin.Exception (Exception)
import qualified Bluefin.Exception as BF
import Bluefin.IO (IOE)
import qualified Bluefin.IO as BF

newtype DbHandle = DbHandle String deriving (Show)

newtype UserId = UserId String deriving (Show, Eq)

newtype User = User String deriving (Show)

data DbEff es = MkDbEff
  { queryImpl :: DbHandle -> UserId -> Eff es User
  }

query :: (e :> es) => DbEff e -> DbHandle -> UserId -> Eff es User
query db dbHandle userId = useImpl $ queryImpl db dbHandle userId

runDbEffIo ::
  forall exEff dbEff es r.
  (exEff :> es, dbEff :> es) =>
  Exception String exEff ->
  IOE dbEff ->
  (forall e. DbEff e -> Eff (e :& es) r) ->
  Eff es r
runDbEffIo ex _ fn =
  useImplIn
    fn
    ( MkDbEff
        { queryImpl = \_ userId -> do
            if userId == UserId "1"
              then pure $ User "Alice"
              else BF.throw ex "not found"
        }
    )

main :: IO ()
main = do
  let dbHandle = DbHandle "db"

  result <- BF.runEff $ \io -> BF.try $ \ex ->
    runDbEffIo ex io $ \db -> do
      u1 <- query db dbHandle (UserId "1")
      BF.effIO io $ print u1
      u2 <- query db dbHandle (UserId "2")
      BF.effIO io $ print u2

  case result of
    Left err -> print err
    Right _ -> print "success"
