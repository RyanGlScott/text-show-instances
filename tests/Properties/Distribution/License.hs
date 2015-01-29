{-|
Module:      Properties.Distribution.License
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'License's.
-}
module Properties.Distribution.License (cabalDistributionLicenseTests) where

import Distribution.License (License)

import Instances.Distribution.License ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.License ()

cabalDistributionLicenseTests :: [TestTree]
cabalDistributionLicenseTests =
    [ testGroup "Text.Show.Text.Distribution.License"
        [ testProperty "License instance" (prop_matchesShow :: Int -> License -> Bool)
        ]
    ]