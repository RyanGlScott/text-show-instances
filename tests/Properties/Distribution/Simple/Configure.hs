{-|
Module:      Properties.Distribution.Simple.Configure
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Simple.Configure@
module of the @Cabal@ library.
-}
module Properties.Distribution.Simple.Configure (cabalDistributionSimpleConfigureTests) where

import Distribution.Simple.Configure (ConfigStateFileError)

import Instances.Distribution.Simple.Configure ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Simple.Configure ()

cabalDistributionSimpleConfigureTests :: [TestTree]
cabalDistributionSimpleConfigureTests =
    [ testGroup "Text.Show.Text.Distribution.Simple.Configure"
        [ testProperty "ConfigStateFileError instance" (prop_matchesShow :: Int -> ConfigStateFileError -> Bool)
        ]
    ]