{-|
Module:      Properties.Data.Semigroup
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for @Semigroup@ data types.
-}
module Properties.Data.Semigroup (semigroupTests) where

import Data.Semigroup (Min, Max, First, Last, WrappedMonoid, Option)

import Instances.Data.Semigroup ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Data.Semigroup ()

semigroupTests :: [TestTree]
semigroupTests =
    [ testGroup "Text.Show.Text.Data.Semigroup"
        [ testProperty "Min Int instance"           (prop_matchesShow :: Int -> Min Int -> Bool)
        , testProperty "Max Int instance"           (prop_matchesShow :: Int -> Max Int -> Bool)
        , testProperty "First Int instance"         (prop_matchesShow :: Int -> First Int -> Bool)
        , testProperty "Last Int Char"              (prop_matchesShow :: Int -> Last Int -> Bool)
        , testProperty "WrappedMonoid Int instance" (prop_matchesShow :: Int -> WrappedMonoid Int -> Bool)
        , testProperty "Option Int instance"        (prop_matchesShow :: Int -> Option Int -> Bool)
        ]
    ]