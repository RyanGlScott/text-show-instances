{-|
Module:      Properties.Distribution.Simple.Hpc
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Simple.Hpc@
module of the @Cabal@ library.
-}
module Properties.Distribution.Simple.Hpc (cabalDistributionSimpleHpcTests) where

import Distribution.Simple.Hpc (Way)

import Instances.Distribution.Simple.Hpc ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Simple.Hpc ()

cabalDistributionSimpleHpcTests :: [TestTree]
cabalDistributionSimpleHpcTests =
    [ testGroup "Text.Show.Text.Distribution.Simple.Hpc"
        [ testProperty "Way instance" (prop_matchesShow :: Int -> Way -> Bool)
        ]
    ]