module Bluefin
  ( -- | Bluefin's effects take place in the 'Eff' monad.  The most
    -- common effects are
    --
    --  * "Bluefin.State", for mutable state
    --  * "Bluefin.Exception", for exceptions
    --  * "Bluefin.Stream", for streams
    --  * "Bluefin.IO", for I/O
  )
where
