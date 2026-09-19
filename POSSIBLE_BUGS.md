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
