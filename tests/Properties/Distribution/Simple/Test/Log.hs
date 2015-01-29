{-|
Module:      Properties.Distribution.Simple.Test.Log
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Simple.Test.Log@
module of the @Cabal@ library.
-}
module Properties.Distribution.Simple.Test.Log (cabalDistributionSimpleTestLogTests) where

import Distribution.Simple.Test.Log (PackageLog, TestLogs, TestSuiteLog)

import Instances.Distribution.Simple.Test.Log ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Simple.Test.Log ()

cabalDistributionSimpleTestLogTests :: [TestTree]
cabalDistributionSimpleTestLogTests =
    [ testGroup "Text.Show.Text.Distribution.Simple.Test.Log"
        [ testProperty "PackageLog instance"   (prop_matchesShow :: Int -> PackageLog -> Bool)
        , testProperty "TestLogs instance"     (prop_matchesShow :: Int -> TestLogs -> Bool)
        , testProperty "TestSuiteLog instance" (prop_matchesShow :: Int -> TestSuiteLog -> Bool)
        ]
    ]