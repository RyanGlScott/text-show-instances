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
#if MIN_VERSION_pretty(1,1,2)
import Spec.Utils (prop_genericShow)
#endif

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
spec = parallel $ do
    describe "Doc" $
        prop "Show instance"                     (prop_matchesShow :: Int -> Doc -> Bool)
    -- TODO: Figure out why this randomly stalls forever
--     describe "renderStyleB" $ do
--         prop "has the same output as renderStyle" prop_renderStyle
    describe "Mode" $ do
        prop "Show instance"                     (prop_matchesShow :: Int -> Mode -> Bool)
#if MIN_VERSION_pretty(1,1,2)
        prop "generic Show"                      (prop_genericShow :: Int -> Mode -> Bool)
#endif
    describe "Style" $ do
        prop "Show instance"                     (prop_matchesShow :: Int -> Style -> Bool)
#if MIN_VERSION_pretty(1,1,2)
        prop "generic Show"                      (prop_genericShow :: Int -> Style -> Bool)
#endif
    describe "TextDetails" $ do
        prop "Show instance"                     (prop_matchesShow :: Int -> TextDetails -> Bool)
#if MIN_VERSION_pretty(1,1,2)
        prop "generic Show"                      (prop_genericShow :: Int -> TextDetails -> Bool)
#endif
#if MIN_VERSION_pretty(1,1,2)
    describe "PrettyLevel" $
        prop "Show instance"                     (prop_matchesShow :: Int -> PrettyLevel -> Bool)
#endif

-- | Verifies that the output of 'renderStyle' and 'renderStyleB' coincides.
-- prop_renderStyle :: Style -> Doc -> Bool
-- prop_renderStyle sty doc = fromString (renderStyle sty doc) == renderStyleB sty doc
