{-|
Module:      Properties.Distribution.Simple.BuildTarget
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Simple.BuildTarget@
module of the @Cabal@ library.
-}
module Properties.Distribution.Simple.BuildTarget (cabalDistributionSimpleBuildTargetTests) where

import Distribution.Simple.BuildTarget (BuildTarget, BuildTargetProblem,
                                        UserBuildTarget, UserBuildTargetProblem)

import Instances.Distribution.Simple.BuildTarget ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Simple.BuildTarget ()

cabalDistributionSimpleBuildTargetTests :: [TestTree]
cabalDistributionSimpleBuildTargetTests =
    [ testGroup "Text.Show.Text.Distribution.Simple.BuildTarget"
        [ testProperty "BuildTarget instance"            (prop_matchesShow :: Int -> BuildTarget -> Bool)
        , testProperty "BuildTargetProblem instance"     (prop_matchesShow :: Int -> BuildTargetProblem -> Bool)
        , testProperty "UserBuildTarget instance"        (prop_matchesShow :: Int -> UserBuildTarget -> Bool)
        , testProperty "UserBuildTargetProblem instance" (prop_matchesShow :: Int -> UserBuildTargetProblem -> Bool)
        ]
    ]