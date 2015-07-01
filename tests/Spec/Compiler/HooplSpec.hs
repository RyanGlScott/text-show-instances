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
spec = parallel $ do
    describe "Label" $
        prop "Show instance" (prop_matchesShow :: Int -> Label -> Bool)
    describe "LabelMap Char" $
        prop "Show instance" (prop_matchesShow :: Int -> LabelMap Char -> Bool)
    describe "LabelSet" $
        prop "Show instance" (prop_matchesShow :: Int -> LabelSet -> Bool)
    describe "Pointed C C Int" $
        prop "Show instance" (prop_matchesShow :: Int -> Pointed C C Int -> Bool)
    describe "Unique" $
        prop "Show instance" (prop_matchesShow :: Int -> Unique -> Bool)
    describe "UniqueMap Char" $
        prop "Show instance" (prop_matchesShow :: Int -> UniqueMap Char -> Bool)
    describe "UniqueSet" $
        prop "Show instance" (prop_matchesShow :: Int -> UniqueSet -> Bool)
    describe "DominatorNode" $
        prop "Show instance" (prop_matchesShow :: Int -> DominatorNode -> Bool)
    describe "DominatorTree" $
        prop "Show instance" (prop_matchesShow :: Int -> DominatorTree -> Bool)
    describe "DPath" $
        prop "Show instance" (prop_matchesShow :: Int -> DPath -> Bool)
