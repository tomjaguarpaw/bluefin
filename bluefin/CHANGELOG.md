## 0.2.1.0

* `Bluefin.Compound`: Add `handleImpl`, `HandleD` and
  `handleMapHandle`

## 0.2.0.0

* Transitive version bump because of choice of different incoherent
  instance for `:>` for better type inference.

## 0.0.17.1

* Documentation only, thanks to @ShilohAlleyne

## 0.0.17.0

* Added `streamConsume`, `cycleToStream`, `takeConsume` (thanks to
  @iteratee)

* Added introduction to effect systems documentation (thanks to
  @ShilohAlleyne)

## 0.0.16.0

* Export `runEff_` from `Bluefin.IO`

## 0.0.15.0

* Add `runEff_` to `Bluefin.Eff`

* Add `ignoreStream` to `Bluefin.Stream`

## 0.0.14.1

* Documentation changes only

## 0.0.14.0

* Add `hGetLine` and `hIsEOF` to `Bluefin.System.IO`

* Add `Bluefin.HandleReader`

## 0.0.13.0

* No release

## 0.0.12.0

* Add `asks` and `local` to `Bluefin.Reader`

## 0.0.11.0

* Add `withEffToIO_`, `useImplUnder`, `makeOp`

* Soft deprecate `withEffToIO`, `useImplWithin`

## 0.0.10.0

* Add `Bluefin.System.IO`

## 0.0.9.0

* Add `instance Handle IOE`

* Add `rethrowIO`

## 0.0.8.0

Add `Bluefin.Consume` and `consumeStream`

## 0.0.7.0

Add `Bluefin.Pipes` and `Bluefin.Pipes.Prelude`, `connectCoroutines`
and `useImplWithin`

## 0.0.6.1

* Documentation improvements

## 0.0.6.0

* Add `withYieldToList`

## 0.0.5.0

* Fix roles on `Eff` (thanks to @Lysxia)

* Add `bracket` (thanks to @Lysxia)

* Document `Jump`

## 0.0.4.3

Improve documentation

## 0.0.4.2

* Depend on `bluefin-internal >= 0.0.4.2` so that Hackage will show
  the `Handle` documentation.

## 0.0.4.1

* Add documentation for `Handle`

## 0.0.4.0

* Add documentation and functions to `Bluefin.Compound`

## 0.0.3.0

* Add `Bluefin.Reader`

## 0.0.2.0

* Add `Bluefin.StateSource`

## 0.0.1.0

* Add `Bluefin.Writer`

## 0.0.0.0

* Initial version
