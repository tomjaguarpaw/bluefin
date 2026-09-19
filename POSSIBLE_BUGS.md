# Possible bugs

These are source-audit leads. Unless stated otherwise, they have not been
verified with a reproducer.

## `runReader` never releases its Vault key

`runReader` creates a fresh key and inserts the Reader value into the
environment Vault, but it never removes that entry when the Reader scope
ends (`bluefin-internal/src/Bluefin/Internal.hs`, in `runReader`). Repeated
short-lived Reader scopes therefore keep every Reader value reachable until
the enclosing `runEff` finishes. This can cause unbounded memory growth and
retain sensitive values beyond their intended lifetime.

## `withEffToIOCloneHandle` shares its cloned environment between calls

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

`Bluefin.Internal.Vault` defines `newtype Key a = MkKey (Vault.Key Any)`
without a role annotation, so `a` is inferred at phantom role. Because this
is an exposed module, client code can use ordinary `coerce` to turn a
`Key a` into `Key b` even when `a` and `b` are unrelated, then use the same
key with the exported `insert` and `lookup` at incompatible types. Vault
issue 56 concludes that a representational role is valid, but also explicitly
notes that a phantom role is wrong. `Key` should therefore have an explicit
representational role rather than the currently inferred phantom role.
