{-|
Module:      Spec.Data.TaggedSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for 'Tagged' values.
-}
module Spec.Data.TaggedSpec (main, spec) where

import Data.Tagged (Tagged)

import Instances.Data.Tagged ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.Data.Tagged ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Tagged" $
    prop "Tagged Char Int instance" (prop_matchesShow :: Int -> Tagged Char Int -> Bool)
