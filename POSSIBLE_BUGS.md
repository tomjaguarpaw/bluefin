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
