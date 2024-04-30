# Bluefin

Bluefin is an effect system for Haskell which allows you, through
value-level handles, to freely mix a variety of effects
including

* [`Bluefin.EarlyReturn`](bluefin/src/Bluefin/EarlyReturn.hs), for early return
* [`Bluefin.Exception`](bluefin/src/Bluefin/Exception.hs), for exceptions
* [`Bluefin.IO`](bluefin/src/Bluefin/IO.hs), for I/O
* [`Bluefin.State`](bluefin/src/Bluefin/State.hs), for mutable state
* [`Bluefin.Stream`](bluefin/src/Bluefin/Stream.hs), for streams

## Introduction

For an introduction to Bluefin, see the docs in the
[`Bluefin`](bluefin/src/Bluefin.hs) module.

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
  rank-2 types](
  https://www.reddit.com/r/haskell/comments/pywuqg/comment/hexo2uu/

* Andrzej Rybczak for his work on effectful

* Francois Pottier for "Wandering through linear types, capabilities,
  and regions"
  <http://pauillac.inria.fr/~fpottier/slides/fpottier-2007-05-linear-bestiary.pdf>

* Jasper van de Jeugt, particularly for promoting the handle pattern
  <https://jaspervdj.be/posts/2018-03-08-handle-pattern.html#fnref2>

* Michael Thompson, for his work on `streaming`

* Ningning Xie and Daan Leijen, for their work on Koka

* Andrej Bauer and Matija Pretnar, for their work on Eff
