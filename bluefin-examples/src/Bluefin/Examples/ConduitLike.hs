{- cabal:
build-depends: base, text, directory, conduit
-}
import Conduit
import qualified Data.Conduit.Combinators as CC

main :: IO ()
main =
  runConduitRes $
       CC.enumFromTo (1 :: Int) 4
    .| awaitForever (\x ->
       bracketP (pure ()) (\_ -> putStrLn "close") 
         (\_ -> CC.enumFromTo (1 :: Int) x) 
       .| takeRes 3)
    .| CC.mapM_ (liftIO . print)
    .| sinkNull

takeRes :: MonadResource m => Int -> ConduitT a a m ()
takeRes 0 = liftResourceT $ do
  is <- getInternalState
  closeInternalState is
  liftIO $ writeIORef is $ ReleaseMap maxBound (minBound + 1) mempty
takeRes n = await >>= \case
  Nothing -> mempty
  Just x -> yield x *> takeRes (n - 1)
