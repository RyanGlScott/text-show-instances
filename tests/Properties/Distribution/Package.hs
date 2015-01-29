{-|
Module:      Properties.Distribution.Package
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Package@
module of the @Cabal@ library.
-}
module Properties.Distribution.Package (cabalDistributionPackageTests) where

import Distribution.Package (Dependency, InstalledPackageId, PackageIdentifier,
                             PackageKey, PackageName)

import Instances.Distribution.Package ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Package ()

cabalDistributionPackageTests :: [TestTree]
cabalDistributionPackageTests =
    [ testGroup "Text.Show.Text.Distribution.Package"
        [ testProperty "Dependency instance"         (prop_matchesShow :: Int -> Dependency -> Bool)
        , testProperty "InstalledPackageId instance" (prop_matchesShow :: Int -> InstalledPackageId -> Bool)
        , testProperty "PackageIdentifier instance"  (prop_matchesShow :: Int -> PackageIdentifier -> Bool)
        , testProperty "PackageKey instance"         (prop_matchesShow :: Int -> PackageKey -> Bool)
        , testProperty "PackageName instance"        (prop_matchesShow :: Int -> PackageName -> Bool)
        ]
    ]