{-|
Module:      Spec.Data.ContainersSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @containers@ library.
-}
module Spec.Data.ContainersSpec (main, spec) where

import Data.Graph (SCC)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq, ViewL, ViewR)
import Data.Set (Set)
import Data.Tree (Tree)

import Instances.Data.Containers ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

import TextShow.Data.Containers ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "IntMap Char" $
        matchesTextShowSpec (Proxy :: Proxy (IntMap Char))
    describe "IntSet" $
        matchesTextShowSpec (Proxy :: Proxy IntSet)
    describe "Map Char Char" $
        matchesTextShowSpec (Proxy :: Proxy (Map Char Char))
    describe "Sequence Char" $
        matchesTextShowSpec (Proxy :: Proxy (Seq Char))
    describe "ViewL Char" $
        matchesTextShowSpec (Proxy :: Proxy (ViewL Char))
    describe "ViewR Char" $
        matchesTextShowSpec (Proxy :: Proxy (ViewR Char))
    describe "SCC Char" $
        matchesTextShowSpec (Proxy :: Proxy (SCC Char))
    describe "Set Char" $
        matchesTextShowSpec (Proxy :: Proxy (Set Char))
    describe "Tree Char" $
        matchesTextShowSpec (Proxy :: Proxy (Tree Char))
