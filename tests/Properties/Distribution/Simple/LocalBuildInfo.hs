{-|
Module:      Properties.Distribution.Simple.LocalBuildInfo
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Simple.LocalBuildInfo@
module of the @Cabal@ library.
-}
module Properties.Distribution.Simple.LocalBuildInfo (cabalDistributionSimpleLocalBuildInfoTests) where

import Distribution.Simple.LocalBuildInfo
    (Component, ComponentLocalBuildInfo, ComponentName, LibraryName, LocalBuildInfo)

import Instances.Distribution.Simple.LocalBuildInfo ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Simple.LocalBuildInfo ()

cabalDistributionSimpleLocalBuildInfoTests :: [TestTree]
cabalDistributionSimpleLocalBuildInfoTests =
    [ testGroup "Text.Show.Text.Distribution.Simple.LocalBuildInfo"
        [ testProperty "Component instance"               (prop_matchesShow :: Int -> Component -> Bool)
        , testProperty "ComponentLocalBuildInfo instance" (prop_matchesShow :: Int -> ComponentLocalBuildInfo -> Bool)
        , testProperty "ComponentName instance"           (prop_matchesShow :: Int -> ComponentName -> Bool)
        , testProperty "LibraryName instance"             (prop_matchesShow :: Int -> LibraryName -> Bool)
        , testProperty "LocalBuildInfo instance"          (prop_matchesShow :: Int -> LocalBuildInfo -> Bool)
        ]
    ]