{-|
Module:      Properties.System.Locale
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'TimeLocale'.
-}
module Properties.System.Locale (oldLocaleTests) where

import Instances.System.Locale ()

import Properties.Utils (prop_matchesShow)

import System.Locale (TimeLocale)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.System.Locale ()

oldLocaleTests :: [TestTree]
oldLocaleTests =
    [ testGroup "Text.Show.Text.System.Locale"
        [ testProperty "TimeLocale instance" (prop_matchesShow :: Int -> TimeLocale -> Bool)
        ]
    ]