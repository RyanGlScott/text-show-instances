{-|
Module:      Properties.Distribution.TestSuite
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.TestSuite@
module of the @Cabal@ library.
-}
module Properties.Distribution.TestSuite (cabalDistributionTestSuiteTests) where

import Distribution.TestSuite (OptionDescr, OptionType, Result)

import Instances.Distribution.TestSuite ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.TestSuite ()

cabalDistributionTestSuiteTests :: [TestTree]
cabalDistributionTestSuiteTests =
    [ testGroup "Text.Show.Text.Distribution.TestSuite"
        [ testProperty "OptionDescr instance" (prop_matchesShow :: Int -> OptionDescr -> Bool)
        , testProperty "OptionType instance"  (prop_matchesShow :: Int -> OptionType -> Bool)
        , testProperty "Result instance"      (prop_matchesShow :: Int -> Result -> Bool)
        ]
    ]