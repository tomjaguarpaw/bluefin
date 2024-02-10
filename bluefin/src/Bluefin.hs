module Bluefin
  ( -- | Bluefin's effects take place in the 'Eff' monad.  The most
    -- common effects are
    --
    --  * "Bluefin.EarlyReturn", for early return
    --  * "Bluefin.Exception", for exceptions
    --  * "Bluefin.IO", for I/O
    --  * "Bluefin.State", for mutable state
    --  * "Bluefin.Stream", for streams
  )
where
