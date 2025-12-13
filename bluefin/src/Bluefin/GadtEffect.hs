module Bluefin.GadtEffect
  ( -- * Introduction

    -- | The Haskell effect systems @effectful@ and @polysemy@ allow
    -- users to define new effects by defining a GADT (generalized
    -- algebraic data type) whose contructors correspond to primitive
    -- operations of the effect, and then creating values of the GADT
    -- and interpreting them in terms of existing effects.  This
    -- module provides Bluefin's equivalent.  In fact, it @effectful@
    -- and @polysemy@ this is essentially the /only/ way you can
    -- create new effects. That's not true for Bluefin. Bluefin
    -- supports a rich collection of ways to create new effects, most
    -- of which are documented at "Bluefin.Compound".  This particular
    -- module might be helpful for users coming from @effectful@ and
    -- @polysemy@, however.

    -- * Example filesystem effect

    -- | First we define a GADT with a constructor for each primitive
    -- operation of the effect we want to define.  Here the primitive
    -- operations are to read a file, write a file and to wrap an
    -- effectful computation in a "trace" block.
    --
    -- @
    -- data FileSystem :: 'Effect' where
    --   ReadFile :: FilePath -> FileSystem m String
    --   WriteFile :: FilePath -> String -> FileSystem m ()
    --   Trace :: String -> m r -> FileSystem m r
    -- @
    --
    -- Then we need to define two instances for @FileSystem@:
    --
    -- @
    -- instance
    --   (e :> es) =>
    --   t'Bluefin.Compound.OneWayCoercible' ('GadtEffect' FileSystem r e) (GadtEffect FileSystem r es)
    --   where
    --   'Bluefin.Compound.oneWayCoercibleImpl' = 'oneWayCoercibleGadtEffectTrustMe' $ \\case
    --     ReadFile path -> ReadFile path
    --     WriteFile path contents -> WriteFile path contents
    --     Trace msg body -> Trace msg (useImpl body)
    --
    -- deriving via
    --   t'Bluefin.Compound.OneWayCoercibleHandle' ('GadtEffect' FileSystem r)
    --   instance
    --     t'Bluefin.Compound.Handle' (GadtEffect FileSystem r)
    -- @
    --
    -- Then we can define functions that implement the primitive
    -- effectful operations for @FileSystem@:
    --
    -- @
    -- readFile ::
    --   (e1 :> es) =>
    --   'Send' FileSystem e1 ->
    --   FilePath ->
    --   Eff es String
    -- readFile fc path =
    --   'send' fc (ReadFile path)
    --
    -- writeFile ::
    --   (e1 :> es) =>
    --   Send FileSystem e1 ->
    --   FilePath ->
    --   String ->
    --   Eff es ()
    -- writeFile fc path content =
    --   send fc (WriteFile path content)
    --
    -- trace ::
    --   (e1 :> es) =>
    --   Send FileSystem e1 ->
    --   String ->
    --   Eff es r ->
    --   Eff es r
    -- trace fc msg body =
    --   send fc (Trace msg body)
    -- @
    --
    -- The instances and primitive effectful operations are
    -- boilerplate.  @effectful@ and @polysemy@ have Template Haskell
    -- for generating their boilerplate
    -- ([@makeEffect@](https://hackage.haskell.org/package/effectful-th/docs/Effectful-TH.html#v:makeEffect)
    -- and
    -- [@makeSem@](https://hackage.haskell.org/package/polysemy-1.9.2.0/docs/Polysemy.html#v:makeSem)
    -- respectively) but there is no such thing for Bluefin yet,
    -- sorry!  Please [open an
    -- issue](https://github.com/tomjaguarpaw/bluefin/issues/new) if
    -- that causes difficulties for you.
    --
    -- Finally we can write a handler for the @'Send' FileSystem@
    -- effect that gives it an interpretation via 'interpret':
    --
    -- @
    -- import System.IO qualified as IO
    --
    -- runFileSystem ::
    --   forall es e1 e2 r.
    --   (e1 :> es, e2 :> es) =>
    --   t'Bluefin.IO.IOE' e1 ->
    --   t'Bluefin.Exception.Exception' t'Control.Exception.IOException' e2 ->
    --   (forall e. 'Send' FileSystem e -> Eff (e :& es) r) ->
    --   Eff es r
    -- runFileSystem io ex = 'interpret' $ \\case
    --   ReadFile path ->
    --     adapt (IO.'System.IO.readFile' path)
    --   WriteFile path contents ->
    --     adapt (IO.'System.IO.writeFile' path contents)
    --   Trace msg body -> do
    --     'Bluefin.IO.effIO' io (putStrLn ("Start: " <> msg))
    --     r <- 'Bluefin.Compound.useImpl' body
    --     effIO io (putStrLn ("End: " <> msg))
    --     pure r
    --   where
    --     -- If you don't want to write this signature you can use
    --     -- {-# LANGUAGE NoMonoLocalBinds #-}
    --     adapt :: (e1 :> es', e2 :> es') => IO r' -> Eff es' r'
    --     adapt m = 'Bluefin.IO.rethrowIO' io ex (effIO io m)
    -- @

    -- * @interpose@ example

    -- | If you're familiar with @effectful@'s @interpose@ function
    -- you may want to use Bluefin's equivalent.  To see how, let's
    -- replicate [@effectful@'s interpose
    -- example](https://hackage-content.haskell.org/package/effectful-core-2.6.1.0/docs/Effectful-Dispatch-Dynamic.html#v:interpose). First
    -- we define a simple effect with three primitive operations:
    --
    -- @
    -- data E :: 'Effect' where
    --   Op1 :: E m ()
    --   Op2 :: E m ()
    --   Op3 :: E m ()
    -- @
    --
    -- Then we define the boilerplate instances
    --
    -- @
    -- instance
    --   (e :> es) =>
    --   t'Bluefin.Compound.OneWayCoercible' ('GadtEffect' E r e) (GadtEffect E r es)
    --   where
    --   'Bluefin.Compound.oneWayCoercibleImpl' = 'oneWayCoercibleGadtEffectTrustMe' $ \\case
    --     Op1 -> Op1
    --     Op2 -> Op2
    --     Op3 -> Op3
    --
    -- deriving via
    --   t'Bluefin.Compound.OneWayCoercibleHandle' (GadtEffect E r)
    --   instance
    --     t'Bluefin.Compound.Handle' (GadtEffect E r)
    -- @
    --
    -- and a handler for the @'Send' E@ effect:
    --
    -- @
    -- runE ::
    --   (e1 :> es) =>
    --   IOE e1 ->
    --   (forall e. Send E e -> Eff (e :& es) r) ->
    --   Eff es r
    -- runE io = interpret $ \\case
    --   Op1 -> effIO io (putStrLn "op1")
    --   Op2 -> effIO io (putStrLn "op2")
    --   Op3 -> effIO io (putStrLn "op3")
    -- @
    --
    -- Before using 'interpose', let's look at a use of its simpler
    -- cousin, 'interpret':
    --
    -- @
    -- augmentOp2Interpret ::
    --   (e1 :> es, e2 :> es) =>
    --   IOE e2 ->
    --   Send E e1 ->
    --   (forall e. Send E e -> Eff (e :& es) r) ->
    --   Eff es r
    -- augmentOp2Interpret io fc = 'interpret' $ \\case
    --   Op2 -> effIO io (putStrLn "augmented op2") >> send fc Op2
    --   op -> 'passthrough' fc op
    -- @
    --
    -- Using 'interpose' is similar:
    --
    -- @
    -- augmentOp2Interpose ::
    --   (e1 :> es, e2 :> es) =>
    --   IOE e2 ->
    --   t'Bluefin.HandleReader.HandleReader' (Send E) e1 ->
    --   Eff es r ->
    --   Eff es r
    -- augmentOp2Interpose io = 'interpose' $ \\fc -> \\case
    --   Op2 -> effIO io (putStrLn "augmented op2") >> send fc Op2
    --   op -> 'passthrough' fc op
    -- @
    --
    -- And now let's see what they each do:
    --
    -- @
    -- example :: IO ()
    -- example = runEff $ \\io -> do
    --   let action fc = do
    --         send fc Op1
    --         send fc Op2
    --         send fc Op3
    --
    --   effIO io (putStrLn "-- interpret:")
    --   runE io $ \\fc -> do
    --     augmentOp2Interpret io fc $ \\fc' -> action fc'
    --
    --   effIO io (putStrLn "-- interpose:")
    --   runE io $ \\fc -> 'Bluefin.HandleReader.runHandleReader' fc $ \\hr -> do
    --     augmentOp2Interpose io hr $ 'Bluefin.HandleReader.asksHandle' hr action
    -- @
    --
    -- @
    -- ghci> example
    -- -- interpret:
    -- op1
    -- augmented op2
    -- op2
    -- op3
    -- -- interpose:
    -- op1
    -- augmented op2
    -- op2
    -- op3
    -- @

    -- * Handle

    Send,

    -- * Effectful operations
    send,
    passthrough,

    -- * Interpretation
    EffectHandler,
    interpret,
    interpose,

    -- * @Effect@
    Effect,

    -- * @GadtEffect@
    GadtEffect,
    oneWayCoercibleGadtEffectTrustMe,
  )
where

import Bluefin.Internal.GadtEffect
