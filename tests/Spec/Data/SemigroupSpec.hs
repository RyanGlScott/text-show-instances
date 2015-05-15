{-|
Module:      Spec.Data.SemigroupSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for @Semigroup@ data types.
-}
module Spec.Data.SemigroupSpec (main, spec) where

import Data.Semigroup (Min, Max, First, Last, WrappedMonoid, Option, Arg)

import Instances.Data.Semigroup ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.Data.Semigroup ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Semigroup" $ do
    prop "Min Int instance"           (prop_matchesShow :: Int -> Min Int -> Bool)
    prop "Max Int instance"           (prop_matchesShow :: Int -> Max Int -> Bool)
    prop "First Int instance"         (prop_matchesShow :: Int -> First Int -> Bool)
    prop "Last Int Char"              (prop_matchesShow :: Int -> Last Int -> Bool)
    prop "WrappedMonoid Int instance" (prop_matchesShow :: Int -> WrappedMonoid Int -> Bool)
    prop "Option Int instance"        (prop_matchesShow :: Int -> Option Int -> Bool)
    prop "Arg Int Int instance"       (prop_matchesShow :: Int -> Arg Int Int -> Bool)
