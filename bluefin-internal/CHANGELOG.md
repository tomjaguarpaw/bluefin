* Added `streamConsume`, `cycleToStream`, `takeConsume` (thanks to
  @iteratee)

* Restrict the kind of the argument to
  `Bluefin.Internal.Exception.Scoped.Exception` and
  `Bluefin.Internal.ConstEffect` to `Effects`

## 0.1.0.0

* Implement `Exception` using `Eff` not naked `IO`

* Remove unused `unsafeRemoveEff`

* Add `unsafeCoerceEff`

* Rename `UnsafeMkException` to `MkException`

* Add type roles for `StateSource`, `State`, `IOE`

* Correct implementation of `have`

* Add `withStateInIO`

* Correct type signature of `newState`

* Add safe scoped exceptions implementation

  * `Bluefin.Internal.Exception.Scoped`

  * `Bluefin.Internal..Scoped`

## 0.0.15.0

* Add `runEff_` and `ignoreStream`

## 0.0.14.0

* Add `hGetLine` and `hIsEOF`

* Add `ConstEffect`, `runConstEffect`

* Add `HandleReader`, `mapHandleReader`, `localHandle`, `askHandle`,
  `runHandleReader`

## 0.0.13.0

* Added some examples

## 0.0.12.0

* Add `asks` and `local`

## 0.0.11.0

* Add `withEffToIO_`, `useImplUnder`, `makeOp`

* Soft deprecate `withEffToIO`, `useImplWithin`

## 0.0.10.0

* Add `Bluefin.Internal.System.IO`

## 0.0.9.0

* Add `instance Handle IOE`

* Add `rethrowIO`

## 0.0.8.0

Add `Consume`, `await`, `consumeStream`, `zipCoroutines`,
`consumeEach`, and associated examples.

## 0.0.7.0

Add `withEffToIO'`, `race`, `connectCoroutines`, `receiveStream`,
`insertManySecond`, `useImplWithin`, `unsafeProvideIO`, and
`Bluefin.Internal.Pipes`

## 0.0.6.1

* Documentation improvements

## 0.0.6.0

* Add `withYieldToList`

## 0.0.5.0

* Fix roles on `Eff` (thanks to @Lysxia)

* Add `bracket` (thanks to @Lysxia)

* Document `Jump`

## 0.0.4.2

* Add documentation for `Handle`

## 0.0.4.0

* Add functions for compound effects

## 0.0.3.0

* Add `Bluefin.Internal.Reader`

## 0.0.2.0

* Add `Bluefin.Internal.StateSource`

## 0.0.1.0

* Add `Bluefin.Internal.Writer`

## 0.0.0.0

* Initial version
