{-|
Module:      Properties.Distribution.PackageDescription
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.PackageDescription@
module of the @Cabal@ library.
-}
module Properties.Distribution.PackageDescription (cabalDistributionPackageDescriptionTests) where

import Distribution.PackageDescription
import Distribution.PackageDescription.Check (PackageCheck)

import Instances.Distribution.PackageDescription ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.PackageDescription ()

cabalDistributionPackageDescriptionTests :: [TestTree]
cabalDistributionPackageDescriptionTests =
    [ testGroup "Text.Show.Text.Distribution.PackageDescription"
        [ testProperty "Benchmark instance"                 (prop_matchesShow :: Int -> Benchmark -> Bool)
        , testProperty "BenchmarkInterface instance"        (prop_matchesShow :: Int -> BenchmarkInterface -> Bool)
        , testProperty "BenchmarkType instance"             (prop_matchesShow :: Int -> BenchmarkType -> Bool)
        , testProperty "BuildInfo instance"                 (prop_matchesShow :: Int -> BuildInfo -> Bool)
        , testProperty "BuildType instance"                 (prop_matchesShow :: Int -> BuildType -> Bool)
        , testProperty "Condition Int instance"             (prop_matchesShow :: Int -> Condition Int -> Bool)
        , testProperty "CondTree Int Int Int instance"      (prop_matchesShow :: Int -> CondTree Int Int Int -> Bool)
        , testProperty "ConfVar instance"                   (prop_matchesShow :: Int -> ConfVar -> Bool)
        , testProperty "Executable instance"                (prop_matchesShow :: Int -> Executable -> Bool)
        , testProperty "Flag instance"                      (prop_matchesShow :: Int -> Flag -> Bool)
        , testProperty "FlagName instance"                  (prop_matchesShow :: Int -> FlagName -> Bool)
        , testProperty "GenericPackageDescription instance" (prop_matchesShow :: Int -> GenericPackageDescription -> Bool)
        , testProperty "Library instance"                   (prop_matchesShow :: Int -> Library -> Bool)
        , testProperty "ModuleReexport instance"            (prop_matchesShow :: Int -> ModuleReexport -> Bool)
        , testProperty "ModuleRenaming instance"            (prop_matchesShow :: Int -> ModuleRenaming -> Bool)
        , testProperty "PackageDescription instance"        (prop_matchesShow :: Int -> PackageDescription -> Bool)
        , testProperty "RepoKind instance"                  (prop_matchesShow :: Int -> RepoKind -> Bool)
        , testProperty "RepoType instance"                  (prop_matchesShow :: Int -> RepoType -> Bool)
        , testProperty "SourceRepo instance"                (prop_matchesShow :: Int -> SourceRepo -> Bool)
        , testProperty "TestSuite instance"                 (prop_matchesShow :: Int -> TestSuite -> Bool)
        , testProperty "TestSuiteInterface instance"        (prop_matchesShow :: Int -> TestSuiteInterface -> Bool)
        , testProperty "TestType instance"                  (prop_matchesShow :: Int -> TestType -> Bool)
        , testProperty "PackageCheck instance"              (prop_matchesShow :: Int -> PackageCheck -> Bool)
        ]
    ]