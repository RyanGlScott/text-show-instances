{-|
Module:      Spec.Compiler.HooplSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for data types in the @hoopl@ library.
-}
module Spec.Compiler.HooplSpec (main, spec) where

import Compiler.Hoopl (Label, LabelMap, LabelSet, Pointed,
                       Unique, UniqueMap, UniqueSet, C)
import Compiler.Hoopl.Passes.Dominator (DominatorNode, DominatorTree, DPath)

import Instances.Compiler.Hoopl ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.Compiler.Hoopl ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Compiler.Hoopl" $ do
    prop "Label instance"           (prop_matchesShow :: Int -> Label -> Bool)
    prop "LabelMap Char instance"   (prop_matchesShow :: Int -> LabelMap Char -> Bool)
    prop "LabelSet instance"        (prop_matchesShow :: Int -> LabelSet -> Bool)
    prop "Pointed C C Int instance" (prop_matchesShow :: Int -> Pointed C C Int -> Bool)
    prop "Unique instance"          (prop_matchesShow :: Int -> Unique -> Bool)
    prop "UniqueMap Char instance"  (prop_matchesShow :: Int -> UniqueMap Char -> Bool)
    prop "UniqueSet instance"       (prop_matchesShow :: Int -> UniqueSet -> Bool)
    prop "DominatorNode instance"   (prop_matchesShow :: Int -> DominatorNode -> Bool)
    prop "DominatorTree instance"   (prop_matchesShow :: Int -> DominatorTree -> Bool)
    prop "DPath instance"           (prop_matchesShow :: Int -> DPath -> Bool)