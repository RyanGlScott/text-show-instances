{-|
Module:      Properties.Text.XHtml
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @xhtml@ library.
-}
module Properties.Text.XHtml (xhtmlTests) where

import Instances.Text.XHtml ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Text.XHtml ()
import Text.XHtml.Frameset (Html, HtmlAttr, HotLink)
import Text.XHtml.Table (HtmlTable)

xhtmlTests :: [TestTree]
xhtmlTests =
    [ testGroup "Text.Show.Text.Text.XHtml"
        [ testProperty "Html instance"      (prop_matchesShow :: Int -> Html -> Bool)
        , testProperty "[Html] instance"    (prop_matchesShow :: Int -> [Html] -> Bool)
        , testProperty "HtmlAttr instance"  (prop_matchesShow :: Int -> HtmlAttr -> Bool)
        , testProperty "HotLink instance"   (prop_matchesShow :: Int -> HotLink -> Bool)
        , testProperty "HtmlTable instance" (prop_matchesShow :: Int -> HtmlTable -> Bool)
        ]
    ]