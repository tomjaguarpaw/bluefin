-- |
--
-- @
-- -- ghci> example
-- -- Triangular number 1 is 0
-- -- Triangular number 2 is 1
-- -- Triangular number 3 is 3
-- -- Triangular number 4 is 6
-- -- Triangular number 5 is 10
-- example :: IO ()
-- example = runEff $ \\io -> evalState @Int 0 $ \\st -> do
--   for_ [1..5] $ \\i -> do
--     n <- get st
--     let msg = "Triangular number " <> show i <> " is " <> show n
--     effIO io (putStrLn msg)
--     st += i
-- @
--
-- For @('Bluefin.Contrib.Operators.Clashing.>>=')@ and
-- @('Bluefin.Contrib.Operators.Clashing./=')@, which clash with
-- operators from 'Prelude' of the same name, see
-- "Bluefin.Contrib.Operators.Clashing".
module Bluefin.Contrib.Operators
  ( (+=),
    (-=),
    (*=),
    (%=),
    (&=),
    (|=),
    (^=),
    (<<=),
  )
where

import Bluefin.Eff (Eff, (:>))
import Bluefin.State (State, modify)
import Data.Bits (Bits, shiftL, xor, (.&.), (.|.))

(+=) :: (e :> es, Num a) => State a e -> a -> Eff es ()
(+=) = opEquals (+)

(-=) :: (e :> es, Num a) => State a e -> a -> Eff es ()
(-=) = opEquals (-)

(*=) :: (e :> es, Num a) => State a e -> a -> Eff es ()
(*=) = opEquals (*)

(%=) :: (e :> es, Integral a) => State a e -> a -> Eff es ()
(%=) = opEquals mod

(&=) :: (e :> es, Bits a) => State a e -> a -> Eff es ()
(&=) = opEquals (.&.)

(|=) :: (e :> es, Bits a) => State a e -> a -> Eff es ()
(|=) = opEquals (.|.)

(^=) :: (e :> es, Bits a) => State a e -> a -> Eff es ()
(^=) = opEquals xor

(<<=) :: (e :> es, Bits a) => State a e -> Int -> Eff es ()
(<<=) = opEquals shiftL

opEquals :: (e :> es) => (s -> p -> s) -> State s e -> p -> Eff es ()
opEquals op st n = modify st (`op` n)
