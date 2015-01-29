{-# LANGUAGE CPP #-}
{-|
Module:      Properties.Text.PrettyPrint
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @pretty@ library.
-}
module Properties.Text.PrettyPrint (prettyTests) where

import Instances.Text.PrettyPrint ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.PrettyPrint.HughesPJ (Doc, Mode, Style, TextDetails, renderStyle)
#if MIN_VERSION_pretty(1,1,2)
import Text.PrettyPrint.HughesPJClass (TextDetails)
#endif
import Text.Show.Text (fromString)
import Text.Show.Text.Text.PrettyPrint (renderStyleB)

-- | Verifies that the output of 'renderStyle' and 'renderStyleB' coincides.
prop_renderStyle :: Style -> Doc -> Bool
prop_renderStyle sty doc = fromString (renderStyle sty doc) == renderStyleB sty doc

prettyTests :: [TestTree]
prettyTests =
    [ testGroup "Text.Show.Text.Text.PrettyPrint"
        [ testProperty "Doc instance"         (prop_matchesShow :: Int -> Doc -> Bool)
        , testProperty "rendetStyleB output"  prop_renderStyle
        , testProperty "Mode instance"        (prop_matchesShow :: Int -> Mode -> Bool)
        , testProperty "Style instance"       (prop_matchesShow :: Int -> Style -> Bool)
        , testProperty "TextDetails instance" (prop_matchesShow :: Int -> TextDetails -> Bool)
#if MIN_VERSION_pretty(1,1,2)
        , testProperty "PrettyLevel instance" (prop_matchesShow :: Int -> PrettyLevel -> Bool)
#endif
        ]
    ]