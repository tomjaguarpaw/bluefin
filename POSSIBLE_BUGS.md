# Possible bugs

These are source-audit leads. Unless stated otherwise, they have not been
verified with a reproducer.

## `runReader` never releases its Vault key

Fixed: bracket key insertion and deletion, including exceptional exit.

`runReader` creates a fresh key and inserts the Reader value into the
environment Vault, but it never removes that entry when the Reader scope
ends (`bluefin-internal/src/Bluefin/Internal.hs`, in `runReader`). Repeated
short-lived Reader scopes therefore keep every Reader value reachable until
the enclosing `runEff` finishes. This can cause unbounded memory growth and
retain sensitive values beyond their intended lifetime.

## `withEffToIOCloneHandle` shares its cloned environment between calls

Fixed: clone the captured environment for each runner invocation.

`withEffToIOCloneHandle` applies `withClonedEnv` outside `withEffToIO_`, so
the generated runner closes over one cloned Vault
(`bluefin-internal/src/Bluefin/Internal/CloneableHandle.hs`). Concurrent
invocations of that runner consequently share the same Vault. In particular,
the `Reader` cloner reuses the existing Reader key, while `local` temporarily
replaces and later restores the whole Vault. Overlapping calls can therefore
observe another call's local Reader value or restore a stale Vault over its
changes. This could leak request-specific values between threads.

## `Pipes.Prelude.replicateM` runs one extra iteration

`replicateM n` iterates over `[0 .. n]` in
`bluefin-internal/src/Bluefin/Internal/Pipes.hs`. For nonnegative `n`, that
range contains `n + 1` elements, so the action runs and yields once more than
the function name and its `Control.Monad.replicateM` analogue imply. The
function is publicly re-exported from `Bluefin.Pipes.Prelude`.

## An async exception can poison a shared `runPureEff` thunk

`runPureEff` is implemented directly with `unsafePerformIO` in
`bluefin-internal/src/Bluefin/Internal.hs`. If evaluation of a shared
`runPureEff` thunk is interrupted by an asynchronous exception, the thunk may
retain that exception rather than the intended value, causing later users of
the same pure value to fail. This can turn cancellation or a timeout during
first evaluation into persistent denial of service for a shared thunk or CAF.
The unmerged `runPureEffAsyncSafe` branches contain work aimed at this case,
but the current implementation does not include it.

## `askCapability` can return a capability beyond its scope

`askCapability` can read an `h e` from a `HandleReader h e`, lift that read
with `useImpl`, and return the capability into a wider effect scope. The
capability can then be used after the handler that introduced `e` has ended.
`bluefin-internal/src/Bluefin/Internal.hs` already labels the operation unsafe
and says it will be removed, and `bluefin-internal/test/Main.hs` contains an
escape reproducer. Until removal, both `askCapability` and its deprecated
`askHandle` alias expose this scope escape.

## `Bluefin.Internal.Vault.Key` has a phantom value role

Fixed: require `vault >= 0.3.2.0` and re-export its types and operations,
removing Bluefin's coercion wrapper entirely. Upstream keys have a
representational value role and an abstract constructor.

`Bluefin.Internal.Vault` defines `newtype Key a = MkKey (Vault.Key Any)`
without a role annotation, so `a` is inferred at phantom role. Because this
is an exposed module, client code can use ordinary `coerce` to turn a
`Key a` into `Key b` even when `a` and `b` are unrelated, then use the same
key with the exported `insert` and `lookup` at incompatible types. Vault
issue 56 concludes that a representational role is valid, but also explicitly
notes that a phantom role is wrong. `Key` should therefore have an explicit
representational role rather than the currently inferred phantom role.

## Downstream `:>` instances can forge effect containment

The effect-subset relation `:>` (also exported as `<:`) is an open type class.
`has` in `bluefin-internal/src/Bluefin/Internal.hs` converts any dictionary for
that class into erased `In` evidence and ultimately uses it to coerce `Eff`
actions and capabilities between scopes. Downstream code can define an
invalid orphan instance, including a sufficiently general incoherent one,
without importing an explicitly unsafe operation. Such an instance could
make an escaped capability usable outside its handler and undermine the
guarantees on which `runPureEff` relies. The relation needs to be sealed, or
its evidence must not be trusted as proof unless it came from Bluefin's own
instances.

## `runReader` may force an environment that is never read

`runReader` inserts its environment into `Data.Vault.Strict` with
`modifyIORef'`. The Vault module documents that it is strict in values, and
the strict `IORef` update evaluates the resulting Vault. Consequently,
entering a Reader scope can evaluate the environment even when the body never
calls `ask`. This differs from normal Reader semantics and from storing the
environment lazily in an `IORef`; an unused bottom or expensive thunk can fail
or run unexpectedly at handler entry.

## Exported `connect` and `head'` always fail

`Bluefin.Internal` is an exposed module and implicitly exports `connect`, but
`connect` is defined only as `error "connect unimplemented, sorry"`.
The exported `head'` function calls `connect`, so it also fails whenever used.
These declarations should either be implemented or removed from the exposed
surface.

## `cycleToStream` busy-loops on an empty input

`cycleToStream f y` is implemented as `forever (inFoldable f y)`. If `f` is
empty, the repeated action performs no effects and never yields, producing a
tight nonproductive loop. A consumer waiting for an element cannot make
progress, and the producer can consume a CPU indefinitely. The
`cycleToYield` alias has the same behavior.

## Generic handle cloning can destroy aliasing between fields

The product `GCloneableHandle` instance clones each field independently in
`bluefin-internal/src/Bluefin/Internal/CloneableHandle.hs`. If two fields in a
compound capability refer to the same `State`, the generic clone allocates a
separate copied `State` for each field. The original fields alias, while the
cloned fields do not, so code that observes or relies on that sharing changes
behavior when run through `withEffToIOCloneHandle`.
