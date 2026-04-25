module Bluefin.Capability.Request
  ( -- | @Request@ allows to yield values and await the result. You
    -- might want to start with "Bluefin.Capability.Yield", which is
    -- the most common way to use @Request@s.

    -- ** Prompt finalization/resource safety

    -- | Bluefin
    -- t'Bluefin.Capability.Yield.Yield' \/ t'Bluefin.Capability.Await.Await' \/ t'Bluefin.Capability.Request.Request'
    -- computations have much better resource safety properties than
    -- Conduit and Pipes.  You can use
    -- @Bluefin.Eff.'Bluefin.Eff.bracket'@ within a streaming
    -- computation and the acquired resource is guaranteed to be
    -- released and the end of the bracket, rather than at the end of
    -- the @ResourceT@ scope as it is the case in Conduit and Pipes.
    -- See the blog post [Bluefin streams finalize
    -- promptly](https://h2.jaguarpaw.co.uk/posts/bluefin-streams-finalize-promptly/)
    -- for more details.

    -- ** Running coroutines that communicate via @Request@s

    -- | Bluefin operations can be executed as coroutines using
    --  'connectRequests' ([Wikipedia
    --  suggests](https://en.wikipedia.org/wiki/Coroutine#Definition_and_types)
    --  that such coroutines are "second-class stackful coroutines").
    --  Two coroutines run in this way communicate synchronously by
    --  using @Request@s to interact with a bi-directional
    --  channel. This means that such coroutines are often run
    --  exclusively for what they communicate via this channel, not
    --  for their return value.
    --
    -- @Request@s used in this way work a bit like UNIX pipes: there
    -- is a downstream consumer and an upstream generator. For every
    -- pair of such communicating coroutines there are two ends,
    -- represented with the capabilities @Request a b@ and @Request b
    -- a@. The first parameter to @Request@ is the type that can be
    -- /sent from/ that end, while the second parameter is the type
    -- that will subsequently be /received by/ that end. This explains
    -- the symmetry in the capabilities: what one end sends the other
    -- receives. The implication is that upstream and downstream
    -- exchange messages with each other at the same
    -- time. Additionally, there is a clear order of communication
    -- from the start (in Bluefin, communication is started by
    -- upstream).
    --
    -- 'request' is the only effectful operation required: a @Request
    -- a b@ capability that represents one end of a channel sends @a@s
    -- and receives a @b@s. For many use cases, upstream does not need
    -- to know anything from downstream (dually: downstream does not
    -- need to communicate anything to upstream) except that
    -- downstream is making a new request, so the capabilities that
    -- describe most channels are \"@Request a ()@\" and \"@Request ()
    -- a@\". Bluefin provides synonyms for these:
    -- @'Bluefin.Capability.Yield.Yield' a@ and
    -- @'Bluefin.Capability.Await.Await' a@, respectively. The
    -- specializations of @request@ for @Yield@ and @Await@ are called
    -- 'Bluefin.Capability.Yield.yield' and
    -- 'Bluefin.Capability.Await.await'.  Coroutines that send data in
    -- only one direction like this can be created using 'awaitYield'.
    --
    -- Because the message exchange occurs synchronously, when yielding,
    -- the upstream will block until the downstream awaits. The converse
    -- is also true: when downstream awaits, it will block until upstream
    -- yields.
    --
    -- Any Bluefin effectful operation that takes a @Request@
    -- capability as an argument can be run as coroutine using
    -- 'connectRequests' by providing a second effectful operation
    -- as its counterpart on the other end of the channel.
    --
    -- For simple applications one may not need @connectRequests@ at
    -- all, because specific handlers are already provided by
    -- Bluefin. See the \"Handlers\" sections of the
    -- "Bluefin.Capability.Yield" and "Bluefin.Capability.Await"
    -- modules.

    -- * Capability
    Request,

    -- * Handlers
    forEach,
    connectRequests,

    -- * Effectful operations
    request,
  )
where

import Bluefin.Internal
