{-# LANGUAGE CPP #-}
{-|
Module:      Spec.Text.PrettyPrintSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for data types in the @pretty@ library.
-}
module Spec.Text.PrettyPrintSpec (main, spec) where

import Instances.Text.PrettyPrint ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.PrettyPrint.HughesPJ (Doc, Mode, Style, TextDetails {-, renderStyle -})
#if MIN_VERSION_pretty(1,1,2)
import Text.PrettyPrint.HughesPJClass (PrettyLevel)
#endif
-- import Text.Show.Text (fromString)
import Text.Show.Text.Text.PrettyPrint () -- (renderStyleB)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Text.PrettyPrint" $ do
    prop "Doc instance"         (prop_matchesShow :: Int -> Doc -> Bool)
    -- TODO: Figure out why this randomly stalls forever
--     prop "renderStyleB output"  prop_renderStyle
    prop "Mode instance"        (prop_matchesShow :: Int -> Mode -> Bool)
    prop "Style instance"       (prop_matchesShow :: Int -> Style -> Bool)
    prop "TextDetails instance" (prop_matchesShow :: Int -> TextDetails -> Bool)
#if MIN_VERSION_pretty(1,1,2)
    prop "PrettyLevel instance" (prop_matchesShow :: Int -> PrettyLevel -> Bool)
#endif

-- | Verifies that the output of 'renderStyle' and 'renderStyleB' coincides.
-- prop_renderStyle :: Style -> Doc -> Bool
-- prop_renderStyle sty doc = fromString (renderStyle sty doc) == renderStyleB sty doc
