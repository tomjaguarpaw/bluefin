# Comparisons of Bluefin and effectful

## Type inference

### effectful

effectful has trouble inferring types.  If I write a throw which is
immediately handled then I get an "overlapping instances" error.

```.hs
-- • Overlapping instances for E.Error () :> (E.Error e0 : es0)
--    arising from a use of ‘E.throwError’
e1 = E.runError (E.throwError ())
```

I can put a partial type signature to try to work out what the type
should be

```.hs
-- • Found type wildcard ‘_’
--     standing for ‘Eff es0 (Either (E.CallStack, e0) a)’
e2 :: _
e2 = E.runError (E.throwError ())
```

and paste in the inferred type.  It still has an overlapping instances
error but I get the clue that `e` should probably be `()`

```.hs
-- • Overlapping instances for E.Error () E.:> (E.Error e : es)
--     arising from a use of ‘E.throwError’
e3 :: E.Eff es (Either (E.CallStack, e) a)
e3 = E.runError (E.throwError ())
```

and indeed choosing `e` to be `()` works:

```
-- Works
e4 :: E.Eff es (Either (E.CallStack, ()) a)
e4 = E.runError (E.throwError ())
```

Alternatively we can apply a type application to `runError`, to
disambiguate. The type application plays a similar role to Bluefin's
effect handle.

```.hs
-- Inferred:
-- e5 :: E.Eff es (Either (E.CallStack, ()) a)
e5 = E.runError @() (E.throwError ())
```

### Bluefin

Bluefin infers the type with no problem

```.hs
-- Inferred:
-- b :: Eff es (Either () a)
b = B.try $ \ex -> B.throw ex ()
```
