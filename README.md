# `text-show-instances`
[![Hackage](https://img.shields.io/hackage/v/text-show-instances.svg)][Hackage: text-show-instances]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/text-show-instances.svg)](http://packdeps.haskellers.com/reverse/text-show-instances)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build](https://img.shields.io/travis/RyanGlScott/text-show-instances.svg)](https://travis-ci.org/RyanGlScott/text-show-instances)

[Hackage: text-show-instances]:
  http://hackage.haskell.org/package/text-show-instances
  "text-show-instances package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

`text-show-instances` is a supplemental library to [`text-show`](https://github.com/RyanGlScott/text-show) that provides additional `Show` instances for data types in common Haskell libraries and GHC dependencies that are not encompassed by `text-show`. Currently, `text-show-instances` covers these libraries:

* [`bifunctors`](http://hackage.haskell.org/package/bifunctors)
* [`binary`](http://hackage.haskell.org/package/binary)
* [`containers`](http://hackage.haskell.org/package/containers)
* [`directory`](http://hackage.haskell.org/package/directory)
* [`haskeline`](http://hackage.haskell.org/package/haskeline)
* [`hoopl`](http://hackage.haskell.org/package/hoopl)
* [`hpc`](http://hackage.haskell.org/package/hpc)
* [`old-locale`](http://hackage.haskell.org/package/old-locale)
* [`old-time`](http://hackage.haskell.org/package/old-time)
* [`pretty`](http://hackage.haskell.org/package/pretty)
* [`random`](http://hackage.haskell.org/package/random)
* [`semigroups`](http://hackage.haskell.org/package/semigroups)
* [`tagged`](http://hackage.haskell.org/package/tagged)
* [`template-haskell`](http://hackage.haskell.org/package/template-haskell)
* [`terminfo`](http://hackage.haskell.org/package/terminfo)
* [`time`](http://hackage.haskell.org/package/time)
* [`transformers`](http://hackage.haskell.org/package/transformers)
* [`unix`](http://hackage.haskell.org/package/unix)
* [`unordered-containers`](http://hackage.haskell.org/package/unordered-containers)
* [`vector`](http://hackage.haskell.org/package/vector)
* [`Win32`](http://hackage.haskell.org/package/Win32)
* [`xhtml`](http://hackage.haskell.org/package/xhtml)

One can use these instances by importing `TextShow.Instances`. Alternatively, there are monomorphic versions of the `showb` function available in the other submodules of `TextShow`.
