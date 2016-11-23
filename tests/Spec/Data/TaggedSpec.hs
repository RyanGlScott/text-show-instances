{-|
Module:      Spec.Data.TaggedSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Tagged' values.
-}
module Spec.Data.TaggedSpec (main, spec) where

import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged)

import Instances.Data.Tagged ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

import TextShow.Data.Tagged ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Tagged Char Int" $
    matchesTextShowSpec (Proxy :: Proxy (Tagged Char Int))
