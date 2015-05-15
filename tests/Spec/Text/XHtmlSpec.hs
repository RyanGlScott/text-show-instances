{-|
Module:      Spec.Text.XHtmlSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
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
spec = parallel . describe "Text.Show.Text.Text.XHtml" $ do
    prop "Html instance"      (prop_matchesShow :: Int -> Html -> Bool)
    prop "[Html] instance"    (prop_matchesShow :: Int -> [Html] -> Bool)
    prop "HtmlAttr instance"  (prop_matchesShow :: Int -> HtmlAttr -> Bool)
    prop "HotLink instance"   (prop_matchesShow :: Int -> HotLink -> Bool)
    prop "HtmlTable instance" (prop_matchesShow :: Int -> HtmlTable -> Bool)
