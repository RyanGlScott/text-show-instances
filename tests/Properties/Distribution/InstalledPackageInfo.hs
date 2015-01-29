{-|
Module:      Properties.Distribution.InstalledPackageInfo
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.InstalledPackageInfo@
module of the @Cabal@ library.
-}
module Properties.Distribution.InstalledPackageInfo (cabalDistributionInstalledPackageInfoTests) where

import Distribution.InstalledPackageInfo
    (ExposedModule, InstalledPackageInfo_, OriginalModule)

import Instances.Distribution.InstalledPackageInfo ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.InstalledPackageInfo ()

cabalDistributionInstalledPackageInfoTests :: [TestTree]
cabalDistributionInstalledPackageInfoTests =
    [ testGroup "Text.Show.Text.Distribution.InstalledPackageInfo"
        [ testProperty "ExposedModule instance"              (prop_matchesShow :: Int -> ExposedModule -> Bool)
        , testProperty "InstalledPackageInfo_ Char instance" (prop_matchesShow :: Int -> InstalledPackageInfo_ Char -> Bool)
        , testProperty "OriginalModule instance"             (prop_matchesShow :: Int -> OriginalModule -> Bool)
        ]
    ]