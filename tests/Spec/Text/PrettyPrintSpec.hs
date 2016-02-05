{-# LANGUAGE CPP #-}
{-|
Module:      Spec.Text.PrettyPrintSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @pretty@ library.
-}
module Spec.Text.PrettyPrintSpec (main, spec) where

import           Instances.Text.PrettyPrint ()

import           Spec.Utils (prop_matchesTextShow)
#if MIN_VERSION_pretty(1,1,2)
import           Spec.Utils (prop_genericTextShow)
#endif

import           Test.Hspec (Spec, describe, hspec, parallel)
import           Test.Hspec.QuickCheck (prop)

import           Text.PrettyPrint.HughesPJ (Doc, Mode, Style, TextDetails {-, renderStyle -})
#if MIN_VERSION_pretty(1,1,2)
import           Text.PrettyPrint.HughesPJClass (PrettyLevel)
#endif
#if MIN_VERSION_pretty(1,1,3)
import qualified Text.PrettyPrint.Annotated.HughesPJ as Annot (Doc)
import           Text.PrettyPrint.Annotated.HughesPJ (AnnotDetails, Span)
import qualified Text.PrettyPrint.Annotated.HughesPJClass as Annot (PrettyLevel)
#endif
-- import           TextShow (fromString)
import           TextShow.Text.PrettyPrint () -- (renderStyleB)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Doc" $
        prop "TextShow instance"                 (prop_matchesTextShow :: Int -> Doc -> Bool)
    -- TODO: Figure out why this randomly stalls forever
--     describe "renderStyleB" $ do
--         prop "has the same output as renderStyle" prop_renderStyle
    describe "Mode" $ do
        prop "TextShow instance"                 (prop_matchesTextShow :: Int -> Mode -> Bool)
#if MIN_VERSION_pretty(1,1,2)
        prop "generic TextShow"                  (prop_genericTextShow :: Int -> Mode -> Bool)
#endif
    describe "Style" $ do
        prop "TextShow instance"                 (prop_matchesTextShow :: Int -> Style -> Bool)
#if MIN_VERSION_pretty(1,1,2)
        prop "generic TextShow"                  (prop_genericTextShow :: Int -> Style -> Bool)
#endif
    describe "TextDetails" $ do
        prop "TextShow instance"                 (prop_matchesTextShow :: Int -> TextDetails -> Bool)
#if MIN_VERSION_pretty(1,1,2)
        prop "generic TextShow"                  (prop_genericTextShow :: Int -> TextDetails -> Bool)
#endif
#if MIN_VERSION_pretty(1,1,2)
    describe "PrettyLevel" $
        prop "TextShow instance"                 (prop_matchesTextShow :: Int -> PrettyLevel -> Bool)
#endif
#if MIN_VERSION_pretty(1,1,3)
    describe "AnnotDetails Int" $
        prop "TextShow instance"                 (prop_matchesTextShow :: Int -> AnnotDetails Int -> Bool)
    describe "Doc Int (annotated)" $
        prop "TextShow instance"                 (prop_matchesTextShow :: Int -> Annot.Doc Int -> Bool)
    describe "PrettyLevel (annotated)" $
        prop "TextShow instance"                 (prop_matchesTextShow :: Int -> Annot.PrettyLevel -> Bool)
    describe "Span Int" $
        prop "TextShow instance"                 (prop_matchesTextShow :: Int -> Span Int -> Bool)
#endif

-- | Verifies that the output of 'renderStyle' and 'renderStyleB' coincides.
-- prop_renderStyle :: Style -> Doc -> Bool
-- prop_renderStyle sty doc = fromString (renderStyle sty doc) == renderStyleB sty doc
