{-|
Module:      Properties.Data.List.NonEmpty
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'NonEmpty' lists.
-}
module Properties.Data.List.NonEmpty (nonEmptyListTests) where

import Data.List.NonEmpty (NonEmpty)

import Instances.Data.List.NonEmpty ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Data.List.NonEmpty ()

nonEmptyListTests :: [TestTree]
nonEmptyListTests =
    [ testGroup "Text.Show.Text.Data.List.NonEmpty"
        [ testProperty "NonEmpty Char instance"           (prop_matchesShow :: Int -> NonEmpty Char -> Bool)
        ]
    ]