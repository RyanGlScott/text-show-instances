{-|
Module:      Properties.Distribution.Utils
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'NubList's and 'NubListR's.
-}
module Properties.Distribution.Utils (cabalDistributionUtilsTests) where

import Distribution.Utils.NubList (NubList, NubListR)

import Instances.Distribution.Utils ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Utils ()

cabalDistributionUtilsTests :: [TestTree]
cabalDistributionUtilsTests =
    [ testGroup "Text.Show.Text.Distribution.Utils"
        [ testProperty "NubList Char instance"  (prop_matchesShow :: Int -> NubList Char -> Bool)
        , testProperty "NubListR Char instance" (prop_matchesShow :: Int -> NubListR Char -> Bool)
        ]
    ]