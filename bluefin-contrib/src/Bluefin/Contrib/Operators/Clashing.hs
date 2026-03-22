module Bluefin.Contrib.Operators.Clashing
  ( (/=),
    (>>=),
  )
where

import Bluefin.Eff (Eff, (:>))
import Bluefin.State (State, modify)
import Data.Bits (Bits, shiftR)
import Prelude hiding ((/=), (>>=))

(/=) :: (e :> es, Fractional a) => State a e -> a -> Eff es ()
(/=) = opEquals (/)

(>>=) :: (e :> es, Bits a) => State a e -> Int -> Eff es ()
(>>=) = opEquals shiftR

opEquals :: (e :> es) => (a -> b -> a) -> State a e -> b -> Eff es ()
opEquals op st n = modify st (`op` n)
