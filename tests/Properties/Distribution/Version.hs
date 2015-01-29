{-|
Module:      Properties.Distribution.Version
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Version@
module of the @Cabal@ library.
-}
module Properties.Distribution.Version (cabalDistributionVersionTests) where

import Distribution.Version (Bound, LowerBound, UpperBound,
                             VersionIntervals, VersionRange)

import Instances.Distribution.Version ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Version ()

cabalDistributionVersionTests :: [TestTree]
cabalDistributionVersionTests =
    [ testGroup "Text.Show.Text.Distribution.Version"
        [ testProperty "Bound instance"            (prop_matchesShow :: Int -> Bound -> Bool)
        , testProperty "LowerBound instance"       (prop_matchesShow :: Int -> LowerBound -> Bool)
        , testProperty "UpperBound instance"       (prop_matchesShow :: Int -> UpperBound -> Bool)
        , testProperty "VersionIntervals instance" (prop_matchesShow :: Int -> VersionIntervals -> Bool)
        , testProperty "VersionRange instance"     (prop_matchesShow :: Int -> VersionRange -> Bool)
        ]
    ]