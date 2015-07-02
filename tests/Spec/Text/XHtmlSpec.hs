{-|
Module:      Spec.Text.XHtmlSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @xhtml@ library.
-}
module Spec.Text.XHtmlSpec (main, spec) where

import Instances.Text.XHtml ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.Text.XHtml ()
import Text.XHtml.Frameset (Html, HtmlAttr, HotLink)
import Text.XHtml.Table (HtmlTable)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Html" $
        prop "Show instance" (prop_matchesShow :: Int -> Html -> Bool)
    describe "[Html]" $
        prop "Show instance" (prop_matchesShow :: Int -> [Html] -> Bool)
    describe "HtmlAttr" $
        prop "Show instance" (prop_matchesShow :: Int -> HtmlAttr -> Bool)
    describe "HotLink" $
        prop "Show instance" (prop_matchesShow :: Int -> HotLink -> Bool)
    describe "HtmlTable" $
        prop "Show instance" (prop_matchesShow :: Int -> HtmlTable -> Bool)
