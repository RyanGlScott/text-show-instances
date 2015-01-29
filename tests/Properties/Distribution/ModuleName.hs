{-|
Module:      Properties.Distribution.ModuleName
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'ModuleName's.
-}
module Properties.Distribution.ModuleName (cabalDistributionModuleNameTests) where

import Distribution.ModuleName (ModuleName)

import Instances.Distribution.ModuleName ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.ModuleName ()

cabalDistributionModuleNameTests :: [TestTree]
cabalDistributionModuleNameTests =
    [ testGroup "Text.Show.Text.Distribution.ModuleName"
        [ testProperty "ModuleName instance" (prop_matchesShow :: Int -> ModuleName -> Bool)
        ]
    ]