{-|
Module:      Properties.Distribution.Simple.Program.GHC
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Simple.Program.GHC@
module of the @Cabal@ library.
-}
module Properties.Distribution.Simple.Program.GHC (cabalDistributionSimpleProgramGHCTests) where

import Distribution.Simple.Program.GHC (GhcDynLinkMode, GhcMode,
                                        GhcOptimisation, GhcOptions)

import Instances.Distribution.Simple.Program.GHC ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Simple.Program.GHC ()

cabalDistributionSimpleProgramGHCTests :: [TestTree]
cabalDistributionSimpleProgramGHCTests =
    [ testGroup "Text.Show.Text.Distribution.Simple.Program.GHC"
        [ testProperty "GhcDynLinkMode instance"  (prop_matchesShow :: Int -> GhcDynLinkMode -> Bool)
        , testProperty "GhcMode instance"         (prop_matchesShow :: Int -> GhcMode -> Bool)
        , testProperty "GhcOptimisation instance" (prop_matchesShow :: Int -> GhcOptimisation -> Bool)
        , testProperty "GhcOptions instance"      (prop_matchesShow :: Int -> GhcOptions -> Bool)
        ]
    ]