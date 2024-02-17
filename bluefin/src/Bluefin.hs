module Bluefin
  ( -- * In brief

    -- | Bluefin is an effect system which allows you to freely mix a
    -- variety of effects, including
    --
    --  * "Bluefin.EarlyReturn", for early return
    --  * "Bluefin.Exception", for exceptions
    --  * "Bluefin.IO", for I/O
    --  * "Bluefin.State", for mutable state
    --  * "Bluefin.Stream", for streams

    -- * Introduction

    -- | Bluefin is a Haskell effect system with a new style of API.
    -- It is distinct from prior effect systems because effects are
    -- accessed explicitly through value-level handles which occur as
    -- arguments to effectful operations. Handles (such as
    -- 'Bluefin.State.State' handles, which allow access to mutable
    -- state) are introduced by handlers (such as
    -- 'Bluefin.State.evalState', which sets the initial state).
    -- Here's an example where a mutable state effect handle, @sn@, is
    -- introduced by its handler, 'Bluefin.State.evalState'.
    --
    -- @
    -- -- If @n < 10@ then add 10 to it, otherwise
    -- -- return it unchanged
    -- example1 :: Int -> Int
    -- example1 n = 'Bluefin.Eff.runPureEff' $
    --   -- Create a new state handle, sn, and
    --   -- initialize the value of the state to n
    --   'Bluefin.State.evalState' n $ \\sn -> do
    --     n' <- 'Bluefin.State.get' sn
    --     when (n' < 10) $
    --       'Bluefin.State.modify' sn (+ 10)
    --     get sn
    -- @
    --
    -- @
    -- >>> example1 5
    -- 15
    -- >>> example1 12
    -- 12
    -- @
    --
    -- The handle @st@ is used in much the same way as an
    -- 'Data.STRef.STRef' or 'Data.IORef.IORef'.
    --
    -- A benefit of value-level effect handles is that it's simple to
    -- have multiple effects of the same type in scope at the same
    -- time, which is not simple with existing effect systems.  Here
    -- is an example with two mutable @Int@ state effects in scope.
    --
    -- @
    -- -- Compare two values and add 10
    -- -- to the smaller
    -- example2 :: (Int, Int) -> (Int, Int)
    -- example2 (m, n) = 'Bluefin.Eff.runPureEff' $
    --   'Bluefin.State.evalState' m $ \\sm -> do
    --     evalState n $ \\sn -> do
    --       do
    --         n' <- 'Bluefin.State.get' sn
    --         m' <- get sm
    --
    --         if n' < m'
    --           then 'Bluefin.State.modify' sn (+ 10)
    --           else modify sm (+ 10)
    --
    --       n' <- get sn
    --       m' <- get sm
    --
    --       pure (n', m')
    -- @
    --
    -- @
    -- >>> example2 (5, 10)
    -- (15, 10)
    -- >>> example2 (30, 0)
    -- (30, 10)
    -- @

    -- ** Effect scoping

    -- | Bluefin's use of the type system is very similar to
    -- 'Control.Monad.ST': it ensures that a handle can never escape
    -- the scope of its handler.  That is, once the handler has
    -- finished running there is no way you can use the handle
    -- anymore.

    -- * Comparison to other effect systems

    -- ** Effectful

    -- Haddock seems to have trouble with italic sections spanning
    -- lines :(

    -- | "/Why not just implement this as an alternative API on top of/
    -- /Effectful?/"
    --
    -- That would be great!  But there are two Bluefin features that I
    -- don't know to implement in terms of Effectful:
    -- 'Bluefin.Bluefin.Compound' effects and
    -- 'Bluefin.Bluefin.Coroutine's.

    -- * Implementation

    -- | Bluefin has a similar implementation style to Effectful.
    -- 'Bluefin.Eff.Eff' is an opaque wrapper around 'IO',
    -- 'Bluefin.State.State' is an opaque wrapper around
    -- 'Data.IORef.IORef', and 'Bluefin.Exception.throw' throws an
    -- actual @IO@ exception.
    --
    -- @
    -- newtype 'Bluefin.Eff.Eff' (es :: 'Bluefin.Eff.Effects') a = 'Bluefin.Internal.UnsafeMkEff' (IO a)
    -- newtype 'Bluefin.State.State' s (st :: Effects) = 'Bluefin.Internal.UnsafeMkState' (IORef s)
    -- @
    --
    -- Types of kind 'Bluefin.Eff.Effects' track which effects can be
    -- used in an operation. Bluefin uses them to ensure that effects
    -- cannot escape the scope of their handler, in the same way that
    -- the type parameter to the 'Control.Monad.ST.ST' monad ensures
    -- that mutable state references cannot escape
    -- 'Control.Monad.ST.runST'.  When the type system indicates that
    -- there are no unhandled effects it is safe to run the underlying
    -- @IO@ action using 'System.IO.Unsafe.unsafePerformIO', which is
    -- the approach taken to implement 'Bluefin.Eff.runPureEff'.

    -- * Tips

    -- | * Use @NoMonoLocalBinds@ and @NoMonomorphismRestriction@ for
    -- better type inference.
    --
    -- * Writing a handler often requires an explicit type signature.

    -- * Example

    -- |
    -- @
    -- countPositivesNegatives :: [Int] -> String
    -- countPositivesNegatives is = 'Bluefin.Eff.runPureEff' $
    --   'Bluefin.State.evalState' (0 :: Int) $ \\positives -> do
    --       r \<- 'Bluefin.Exception.try' $ \\ex ->
    --           evalState (0 :: Int) $ \\negatives -> do
    --               for_ is $ \\i -> do
    --                   case compare i 0 of
    --                       GT -> 'Bluefin.State.modify' positives (+ 1)
    --                       EQ -> throw ex ()
    --                       LT -> modify negatives (+ 1)
    --
    --               p <- 'Bluefin.State.get' positives
    --               n <- get negatives
    --
    --               pure $
    --                 "Positives: "
    --                   ++ show p
    --                   ++ ", negatives "
    --                   ++ show n
    --
    --       case r of
    --           Right r' -> pure r'
    --           Left () -> do
    --               p <- get positives
    --               pure $
    --                 "We saw a zero, but before that there were "
    --                   ++ show p
    --                   ++ " positives"
    -- @
  )
where
