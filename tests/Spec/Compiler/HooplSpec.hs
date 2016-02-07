{-|
Module:      Spec.Compiler.HooplSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @hoopl@ library.
-}
module Spec.Compiler.HooplSpec (main, spec) where

import Compiler.Hoopl (Label, LabelMap, LabelSet, Pointed,
                       Unique, UniqueMap, UniqueSet, C)
import Compiler.Hoopl.Passes.Dominator (DominatorNode, DominatorTree, DPath)

import Instances.Compiler.Hoopl ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow.Compiler.Hoopl ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Label" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Label -> Bool)
    describe "LabelMap Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> LabelMap Char -> Bool)
    describe "LabelSet" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> LabelSet -> Bool)
    describe "Pointed C C Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Pointed C C Int -> Bool)
    describe "Unique" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Unique -> Bool)
    describe "UniqueMap Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> UniqueMap Char -> Bool)
    describe "UniqueSet" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> UniqueSet -> Bool)
    describe "DominatorNode" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DominatorNode -> Bool)
    describe "DominatorTree" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DominatorTree -> Bool)
    describe "DPath" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DPath -> Bool)
