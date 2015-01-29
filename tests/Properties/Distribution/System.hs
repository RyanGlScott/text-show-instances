{-|
Module:      Properties.Distribution.System
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.System@
module of the @Cabal@ library.
-}
module Properties.Distribution.System (cabalDistributionSystemTests) where

import Distribution.System (Arch, OS, Platform)

import Instances.Distribution.System ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.System ()

cabalDistributionSystemTests :: [TestTree]
cabalDistributionSystemTests =
    [ testGroup "Text.Show.Text.Distribution.System"
        [ testProperty "Arch instance"     (prop_matchesShow :: Int -> Arch -> Bool)
        , testProperty "OS instance"       (prop_matchesShow :: Int -> OS -> Bool)
        , testProperty "Platform instance" (prop_matchesShow :: Int -> Platform -> Bool)
        ]
    ]