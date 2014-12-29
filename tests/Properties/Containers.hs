{-|
Module:      Properties.Containers
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @containers@ library.
-}
module Properties.Containers (containersTests) where

import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Sequence (Seq, ViewL, ViewR)
import Data.Set (Set)
import Data.Tree (Tree)

import Instances.Containers ()

import Properties.Utils (prop_matchesShow)

import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Data.Containers ()

containersTests :: [TestTree]
containersTests =
    [ testGroup "Text.Show.Text.Data.Containers"
        [ testProperty "IntMap Char instance"               (prop_matchesShow :: Int -> IntMap Char -> Bool)
        , testProperty "IntSet instance"                    (prop_matchesShow :: Int -> IntSet -> Bool)
        , testProperty "Map Char Char instance"             (prop_matchesShow :: Int -> Map Char Char -> Bool)
        , testProperty "Sequence Char"                      (prop_matchesShow :: Int -> Seq Char -> Bool)
        , testProperty "ViewL Int instance"                 (prop_matchesShow :: Int -> ViewL Int -> Bool)
        , testProperty "ViewR Int instance"                 (prop_matchesShow :: Int -> ViewR Int -> Bool)
        , testProperty "Set Char instance"                  (prop_matchesShow :: Int -> Set Char -> Bool)
        , testProperty "Tree Char instance"                 (prop_matchesShow :: Int -> Tree Char -> Bool)
        ]
    ]