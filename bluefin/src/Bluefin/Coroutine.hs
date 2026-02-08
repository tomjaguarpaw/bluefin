module Bluefin.Coroutine
  ( -- | @Coroutine@ allows to yield values and receive results back.
    -- [Wikipedia
    -- suggests](https://en.wikipedia.org/wiki/Coroutine#Definition_and_types)
    -- that Bluefin's coroutines are "second-class stackful
    -- coroutines".  This module is not documented much yet.  You might
    -- want to start with "Bluefin.Stream", which is the most common
    -- way to use coroutines.

    -- ** Prompt finalization/resource safety

    -- | Bluefin
    -- 'Bluefin.Stream.Stream'\/'Bluefin.Consume.Consume'\/'Bluefin.Coroutine.Coroutine'
    -- computations have much better resource safety properties than
    -- Conduit and Pipes.  You can use
    -- @Bluefin.Eff.'Bluefin.Eff.bracket'@ within a streaming
    -- computation and the acquired resource is guaranteed to be
    -- released and the end of the bracket, rather than at the end of
    -- the @ResourceT@ scope as it is the case in Conduit and Pipes.
    -- See the blog post [Bluefin streams finalize
    -- promptly](https://h2.jaguarpaw.co.uk/posts/bluefin-streams-finalize-promptly/)
    -- for more details.

    -- * Handle
    Coroutine,

    -- * Handlers
    forEach,
    connectCoroutines,

    -- * Effectful operations
    yieldCoroutine,
  )
where

import Bluefin.Internal
