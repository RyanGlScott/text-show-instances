{-# LANGUAGE CPP #-}
{-|
Module:      Properties.Text.PrettyPrint
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'StdGen' values.
-}
module Properties.Text.PrettyPrint (prettyTests) where

import Instances.Text.PrettyPrint ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.PrettyPrint.HughesPJ (Doc, Mode, Style, TextDetails)
#if MIN_VERSION_pretty(1,1,2)
import Text.PrettyPrint.HughesPJClass (TextDetails)
#endif
import Text.Show.Text.Text.PrettyPrint ()

prettyTests :: [TestTree]
prettyTests =
    [ testGroup "Text.Show.Text.Text.PrettyPrint"
        [ testProperty "Doc instance"         (prop_matchesShow :: Int -> Doc -> Bool)
        , testProperty "Mode instance"        (prop_matchesShow :: Int -> Mode -> Bool)
        , testProperty "Style instance"       (prop_matchesShow :: Int -> Style -> Bool)
        , testProperty "TextDetails instance" (prop_matchesShow :: Int -> TextDetails -> Bool)
#if MIN_VERSION_pretty(1,1,2)
        , testProperty "PrettyLevel instance" (prop_matchesShow :: Int -> PrettyLevel -> Bool)
#endif
        ]
    ]