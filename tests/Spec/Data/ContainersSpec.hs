{-|
Module:      Spec.Data.ContainersSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @containers@ library.
-}
module Spec.Data.ContainersSpec (main, spec) where

import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Sequence (Seq, ViewL, ViewR)
import Data.Set (Set)
import Data.Tree (Tree)

import Instances.Data.Containers ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import TextShow.Data.Containers ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "IntMap Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> IntMap Char -> Bool)
    describe "IntSet" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> IntSet -> Bool)
    describe "Map Char Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Map Char Char -> Bool)
    describe "Sequence Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Seq Char -> Bool)
    describe "ViewL Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ViewL Char -> Bool)
    describe "ViewR Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ViewR Char -> Bool)
    describe "Set Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Set Char -> Bool)
    describe "Tree Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Tree Char -> Bool)
