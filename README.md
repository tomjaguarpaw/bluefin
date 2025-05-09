# Bluefin [![Hackage version](https://img.shields.io/hackage/v/bluefin.svg?label=Hackage)](https://hackage.haskell.org/package/bluefin)[![Stackage version](https://www.stackage.org/package/bluefin/badge/nightly?label=Stackage)](https://www.stackage.org/package/bluefin)[![Build status](https://img.shields.io/github/actions/workflow/status/tomjaguarpaw/bluefin/ci.yml?branch=master)](https://github.com/tomjaguarpaw/bluefin/actions)

Bluefin is an effect system for Haskell which allows you, through
value-level handles, to freely mix a variety of effects
including

* [`Bluefin.EarlyReturn`](https://hackage.haskell.org/package/bluefin/docs/Bluefin-EarlyReturn.html), for early return
* [`Bluefin.Exception`](https://hackage.haskell.org/package/bluefin/docs/Bluefin-Exception.html), for exceptions
* [`Bluefin.IO`](https://hackage.haskell.org/package/bluefin/docs/Bluefin-IO.html), for I/O
* [`Bluefin.State`](https://hackage.haskell.org/package/bluefin/docs/Bluefin-State.html), for mutable state
* [`Bluefin.Stream`](https://hackage.haskell.org/package/bluefin/docs/Bluefin-Stream.html), for streams

## Introduction

For an introduction to Bluefin, see the docs in the
[`Bluefin`](https://hackage.haskell.org/package/bluefin/docs/Bluefin.html) module.

## Examples

There is an `bluefin-examples` package which you can see in this
repository at
[`bluefin-examples/src/Bluefin/Examples`](bluefin-examples/src/Bluefin/Examples).

## Contact

If you have a question about Bluefin, you think you might have found a
bug, you're stuck on something, or you want to make contact for any
other reason then either [open an
issue](https://github.com/tomjaguarpaw/bluefin/issues) or [start a
discussion](https://github.com/tomjaguarpaw/bluefin/discussions),
whichever you prefer.

## Acknowledgements

Tom Ellis would like to thank many individuals for their work related
to effect systems.  Without the work of these individuals, Bluefin
would not exist.

* Oleg Kiselyov, particularly for his work on effects and delimited
  continuations

* Michael Snoyman, particularly for his work on conduit and the
  `ReaderT` `IO` pattern

* Gabriella Gonzalez, particularly for her work on pipes

* Alexis King, particularly for her work on effect systems and delimited
  continuations

* David Feuer, particularly for [his observation about handlers and
  rank-2
  types](https://www.reddit.com/r/haskell/comments/pywuqg/comment/hexo2uu/)

* Andrzej Rybczak for his work on effectful

* Francois Pottier for "[Wandering through linear types, capabilities,
  and
  regions](http://pauillac.inria.fr/~fpottier/slides/fpottier-2007-05-linear-bestiary.pdf)"

* Jasper van de Jeugt, particularly for promoting [the handle
  pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html#fnref2)

* Michael Thompson, for his work on `streaming`

* Ningning Xie and Daan Leijen, for their work on Koka

* Andrej Bauer and Matija Pretnar, for their work on Eff

* Sjoerd Visscher, for his work on
  [`effects`](https://hackage.haskell.org/package/effects)
