{-|
Module:      Properties.Compiler.Hoopl
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @hoopl@ library.
-}
module Properties.Compiler.Hoopl (hooplTests) where

import Compiler.Hoopl (Label, LabelMap, LabelSet, Pointed, UniqueMap, UniqueSet, C)
import Compiler.Hoopl.Passes.Dominator (DominatorNode, DominatorTree, DPath)

import Instances.Compiler.Hoopl ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Compiler.Hoopl ()

hooplTests :: [TestTree]
hooplTests =
    [ testGroup "Text.Show.Text.Compiler.Hoopl"
        [ testProperty "Label instance"           (prop_matchesShow :: Int -> Label -> Bool)
        , testProperty "LabelMap Char instance"   (prop_matchesShow :: Int -> LabelMap Char -> Bool)
        , testProperty "LabelSet instance"        (prop_matchesShow :: Int -> LabelSet -> Bool)
        , testProperty "Pointed C C Int instance" (prop_matchesShow :: Int -> Pointed C C Int -> Bool)
        , testProperty "UniqueMap Char instance"  (prop_matchesShow :: Int -> UniqueMap Char -> Bool)
        , testProperty "UniqueSet instance"       (prop_matchesShow :: Int -> UniqueSet -> Bool)
        , testProperty "DominatorNode instance"   (prop_matchesShow :: Int -> DominatorNode -> Bool)
        , testProperty "DominatorTree instance"   (prop_matchesShow :: Int -> DominatorTree -> Bool)
        , testProperty "DPath instance"           (prop_matchesShow :: Int -> DPath -> Bool)
        ]
    ]