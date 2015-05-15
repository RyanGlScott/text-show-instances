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
spec = parallel . describe "Text.Show.Text.Data.Containers" $ do
    prop "IntMap Char instance"   (prop_matchesShow :: Int -> IntMap Char -> Bool)
    prop "IntSet instance"        (prop_matchesShow :: Int -> IntSet -> Bool)
    prop "Map Char Char instance" (prop_matchesShow :: Int -> Map Char Char -> Bool)
    prop "Sequence Char"          (prop_matchesShow :: Int -> Seq Char -> Bool)
    prop "ViewL Int instance"     (prop_matchesShow :: Int -> ViewL Int -> Bool)
    prop "ViewR Int instance"     (prop_matchesShow :: Int -> ViewR Int -> Bool)
    prop "Set Char instance"      (prop_matchesShow :: Int -> Set Char -> Bool)
    prop "Tree Char instance"     (prop_matchesShow :: Int -> Tree Char -> Bool)
