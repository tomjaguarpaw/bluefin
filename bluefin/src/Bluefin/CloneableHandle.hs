-- | @Bluefin.CloneableHandle@ defines the 'CloneableHandle' class,
-- whose purpose is to support 'withEffToIOCloneHandle'.

module Bluefin.CloneableHandle
  ( -- | 'withEffToIOCloneHandle' is an @IO@ unlifting function that
    -- clones its handle each time it runs @Eff@ in @IO@.  This is
    -- convenient when the unlifting function is being used to fork
    -- threads, since Bluefin state is not threadsafe.  Be careful
    -- when you use it, because it can be used to throw away the
    -- effect tag on a Bluefin @Eff@ action due to this part of
    -- its type (here throwing away @e@):
    --
    -- @
    -- (forall e. IOE e -> h e -> Eff e r) -> IO r
    -- @
    --
    -- It is only safely used when you do not allow any effects to
    -- escape their scope, so we suggest that you use it sparingly to
    -- define reusable combinators which themselves are safe.  For
    -- example, here is how you could write an equivalent of @async@'s
    -- @race@ primitive:
    --
    -- @
    -- bluefinRace ::
    --   ('CloneableHandle' h, e1 :> es) =>
    --   t'Bluefin.IO.IOE' e1 ->
    --   h es ->
    --   (forall e. IOE e -> h e -> t'Bluefin.Eff.Eff' e r) ->
    --   (forall e. IOE e -> h e -> Eff e r) ->
    --   Eff es r
    -- bluefinRace io h m1 m2 = withEffToIOCloneHandle io h $ \\runInIO -> do
    --  either id id
    --    \<$\> Control.Concurrent.Async.race
    --      (runInIO $ \\io' h' -> m1 io' h')
    --      (runInIO $ \\io' h' -> m2 io' h')
    -- @
    --
    -- Then you can safely use it to race Bluefin @Eff@ actions:
    --
    -- @
    -- example :: IO ()
    -- example = 'Bluefin.Eff.runEff_' $ \\io -> 'Bluefin.State.evalState' 0 $ \\st -> do
    --   r \<- 'Bluefin.Exception.try' $ \\ex -> do
    --     bluefinRace
    --       io
    --       (MkMyHandle ('Bluefin.Handle.mapHandle' ex) (mapHandle st))
    --       ( \\_ (MkMyHandle ex' st') -> do
    --           'Bluefin.State.modify' st' (subtract 2000)
    --           'Bluefin.Exception.throw' ex' "Aborting from branch 1"
    --       )
    --       ( \\_ (MkMyHandle _ st') -> do
    --           modify st' (+ 3000)
    --           pure (2 :: Int)
    --       )
    --
    --   s <- 'Bluefin.State.get' st
    --   'Bluefin.IO.effIO' io (print r)
    --   effIO io (putStrLn ("State started at 0 and was cloned. Now: " <> show s))
    -- @
    --
    -- You can see from the output that the actions were raced as
    -- expected, and the @State@ was cloned so that changes to it in
    -- the branches of @race@ did not affect the original @State@.
    --
    -- @
    -- -- Run one time (the first thread was faster)
    -- ghci> example
    -- Right 2
    -- State started at 0 and was cloned. Now: 0
    -- -- Run another time (the second thread was faster)
    -- ghci> example
    -- Left "Aborting from branch 1"
    -- State started at 0 and was cloned. Now: 0
    -- @
    withEffToIOCloneHandle,

    -- * @CloneableHandle@
    CloneableHandle,
    GenericCloneableHandle(MkGenericCloneableHandle),
    GCloneableHandle,

    -- * @GHC.Generics@ re-exports
    Generic1,
  )
where

import Bluefin.Internal.CloneableHandle
