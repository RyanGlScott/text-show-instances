{-|
Module:      Spec.Data.ContainersSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
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

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import Text.Show.Text.Data.Containers ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "IntMap Char" $
        prop "Show instance" (prop_matchesShow :: Int -> IntMap Char -> Bool)
    describe "IntSet" $
        prop "Show instance" (prop_matchesShow :: Int -> IntSet -> Bool)
    describe "Map Char Char" $
        prop "Show instance" (prop_matchesShow :: Int -> Map Char Char -> Bool)
    describe "Sequence Char" $
        prop "Show instance" (prop_matchesShow :: Int -> Seq Char -> Bool)
    describe "ViewL Char" $
        prop "Show instance" (prop_matchesShow :: Int -> ViewL Char -> Bool)
    describe "ViewR Char" $
        prop "Show instance" (prop_matchesShow :: Int -> ViewR Char -> Bool)
    describe "Set Char" $
        prop "Show instance" (prop_matchesShow :: Int -> Set Char -> Bool)
    describe "Tree Char" $
        prop "Show instance" (prop_matchesShow :: Int -> Tree Char -> Bool)