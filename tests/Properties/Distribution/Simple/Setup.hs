{-|
Module:      Properties.Distribution.Simple.Setup
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Simple.Setup@
module of the @Cabal@ library.
-}
module Properties.Distribution.Simple.Setup (cabalDistributionSimpleSetupTests) where

import Distribution.Simple.Setup
    (BuildFlags, CleanFlags, ConfigFlags, CopyFlags, Flag, HaddockFlags, HscolourFlags,
     InstallFlags, RegisterFlags, ReplFlags, SDistFlags, TestShowDetails)

import Instances.Distribution.Simple.Setup ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Simple.Setup ()

cabalDistributionSimpleSetupTests :: [TestTree]
cabalDistributionSimpleSetupTests =
    [ testGroup "Text.Show.Text.Distribution.Simple.Setup"
        [ testProperty "BuildFlags instance"      (prop_matchesShow :: Int -> BuildFlags -> Bool)
        , testProperty "CleanFlags instance"      (prop_matchesShow :: Int -> CleanFlags -> Bool)
        , testProperty "ConfigFlags instance"     (prop_matchesShow :: Int -> ConfigFlags -> Bool)
        , testProperty "CopyFlags instance"       (prop_matchesShow :: Int -> CopyFlags -> Bool)
        , testProperty "Flag Int instance"        (prop_matchesShow :: Int -> Flag Int -> Bool)
        , testProperty "HaddockFlags instance"    (prop_matchesShow :: Int -> HaddockFlags -> Bool)
        , testProperty "HscolourFlags instance"   (prop_matchesShow :: Int -> HscolourFlags -> Bool)
        , testProperty "InstallFlags instance"    (prop_matchesShow :: Int -> InstallFlags -> Bool)
        , testProperty "RegisterFlags instance"   (prop_matchesShow :: Int -> RegisterFlags -> Bool)
        , testProperty "ReplFlags instance"       (prop_matchesShow :: Int -> ReplFlags -> Bool)
        , testProperty "SDistFlags instance"      (prop_matchesShow :: Int -> SDistFlags -> Bool)
        , testProperty "TestShowDetails instance" (prop_matchesShow :: Int -> TestShowDetails -> Bool)
        ]
    ]