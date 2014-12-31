# `text-show-instances` [![Hackage version](https://img.shields.io/hackage/v/text-show-instances.svg)](http://hackage.haskell.org/package/text-show-instances) [![Build Status](https://travis-ci.org/RyanGlScott/text-show-instances.svg)](https://travis-ci.org/RyanGlScott/text-show-instances)

`text-show-instances` is a supplemental library to [`text-show`](https://github.com/RyanGlScott/text-show) that provides additional `Show` instances for data types in common Haskell libraries not encompassed by `text-show`. Currently, `text-show-instances` covers these libraries:

* [`containers`](http://hackage.haskell.org/package/containers)
* [`directory`](http://hackage.haskell.org/package/directory)
* [`hpc`](http://hackage.haskell.org/package/hpc)
* [`old-locale`](http://hackage.haskell.org/package/old-locale)
* [`old-time`](http://hackage.haskell.org/package/old-time)
* [`random`](http://hackage.haskell.org/package/random)
* [`semigroups`](http://hackage.haskell.org/package/semigroups)
* [`tagged`](http://hackage.haskell.org/package/tagged)
* [`time`](http://hackage.haskell.org/package/time)
* [`transformers`](http://hackage.haskell.org/package/transformers)
* [`unix`](http://hackage.haskell.org/package/unix)
* [`unordered-containers`](http://hackage.haskell.org/package/unordered-containers)
* [`vector`](http://hackage.haskell.org/package/vector)
* [`xhtml`](http://hackage.haskell.org/package/xhtml)

One can use these instances by importing `Text.Show.Text.Instances`. Alternatively, there are monomorphic versions of the `showb` function available in the other submodules of `Text.Show.Text`.