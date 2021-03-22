{-# LANGUAGE CPP #-}
{-|
Module:      Spec.Text.PrettyPrintSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @pretty@ library.
-}
module Spec.Text.PrettyPrintSpec (main, spec) where

import           Data.Proxy (Proxy(..))

import           Instances.Text.PrettyPrint ()

import           Spec.Utils (matchesTextShowSpec)
#if MIN_VERSION_pretty(1,1,2)
import           Spec.Utils (genericTextShowSpec)
#endif

import           Test.Hspec (Spec, describe, hspec, parallel)
-- import           Test.Hspec.QuickCheck (prop)

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
        matchesTextShowSpec (Proxy :: Proxy Doc)
    -- TODO: Figure out why this randomly stalls forever
--     describe "renderStyleB" $ do
--         prop "has the same output as renderStyle" prop_renderStyle
    describe "Mode" $ do
        let p :: Proxy Mode
            p = Proxy
        matchesTextShowSpec p
#if MIN_VERSION_pretty(1,1,2)
        genericTextShowSpec p
#endif
    describe "Style" $ do
        let p :: Proxy Style
            p = Proxy
        matchesTextShowSpec p
#if MIN_VERSION_pretty(1,1,2)
        genericTextShowSpec p
#endif
    describe "TextDetails" $ do
        let p :: Proxy TextDetails
            p = Proxy
        matchesTextShowSpec p
#if MIN_VERSION_pretty(1,1,2)
        genericTextShowSpec p
#endif
#if MIN_VERSION_pretty(1,1,2)
    describe "PrettyLevel" $
        matchesTextShowSpec (Proxy :: Proxy PrettyLevel)
#endif
#if MIN_VERSION_pretty(1,1,3)
    describe "AnnotDetails Int" $
        matchesTextShowSpec (Proxy :: Proxy (AnnotDetails Int))
    describe "Doc Int (annotated)" $
        matchesTextShowSpec (Proxy :: Proxy (Annot.Doc Int))
    describe "PrettyLevel (annotated)" $
        matchesTextShowSpec (Proxy :: Proxy Annot.PrettyLevel)
    describe "Span Int" $
        matchesTextShowSpec (Proxy :: Proxy (Span Int))
#endif

-- | Verifies that the output of 'renderStyle' and 'renderStyleB' coincides.
-- prop_renderStyle :: Style -> Doc -> Expectation
-- prop_renderStyle sty doc = fromString (renderStyle sty doc) `shouldBe` renderStyleB sty doc
