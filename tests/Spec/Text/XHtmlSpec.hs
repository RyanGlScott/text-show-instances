{-|
Module:      Spec.Text.XHtmlSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @xhtml@ library.
-}
module Spec.Text.XHtmlSpec (main, spec) where

import Instances.Text.XHtml ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow.Text.XHtml ()
import Text.XHtml.Frameset (Html, HtmlAttr, HotLink)
import Text.XHtml.Table (HtmlTable)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Html" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Html -> Bool)
    describe "[Html]" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> [Html] -> Bool)
    describe "HtmlAttr" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> HtmlAttr -> Bool)
    describe "HotLink" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> HotLink -> Bool)
    describe "HtmlTable" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> HtmlTable -> Bool)
