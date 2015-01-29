{-|
Module:      Properties.Distribution.Verbosity
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'Verbosity' values.
-}
module Properties.Distribution.Verbosity (cabalDistributionVerbosityTests) where

import Distribution.Verbosity (Verbosity)

import Instances.Distribution.Verbosity ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Verbosity ()

cabalDistributionVerbosityTests :: [TestTree]
cabalDistributionVerbosityTests =
    [ testGroup "Text.Show.Text.Distribution.Verbosity"
        [ testProperty "Verbosity instance" (prop_matchesShow :: Int -> Verbosity -> Bool)
        ]
    ]