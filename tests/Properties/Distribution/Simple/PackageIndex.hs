{-|
Module:      Properties.Distribution.Simple.PackageIndex
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Simple.PackageIndex@
module of the @Cabal@ library.
-}
module Properties.Distribution.Simple.PackageIndex (cabalDistributionSimplePackageIndexTests) where

import Distribution.Simple.PackageIndex (InstalledPackageIndex)

import Instances.Distribution.InstalledPackageInfo ()
import Instances.Distribution.Simple.PackageIndex  ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.InstalledPackageInfo ()
import Text.Show.Text.Distribution.Simple.PackageIndex  ()

cabalDistributionSimplePackageIndexTests :: [TestTree]
cabalDistributionSimplePackageIndexTests =
    [ testGroup "Text.Show.Text.Distribution.Simple.PackageIndex"
        [ testProperty "InstalledPackageIndex instance" (prop_matchesShow :: Int -> InstalledPackageIndex -> Bool)
        ]
    ]