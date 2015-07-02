{-|
Module:      Spec.Data.SemigroupSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for @Semigroup@ data types.
-}
module Spec.Data.SemigroupSpec (main, spec) where

import Data.Semigroup (Min, Max, First, Last, WrappedMonoid, Option, Arg)

import Instances.Data.Semigroup ()

import Spec.Utils (prop_matchesShow, prop_genericShow, prop_genericShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.Data.Semigroup ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Min Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Min Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Min Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> Min Int -> Bool)
    describe "Max Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Max Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Max Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> Max Int -> Bool)
    describe "First Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> First Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> First Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> First Int -> Bool)
    describe "Last Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Last Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Last Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> Last Int -> Bool)
    describe "WrappedMonoid Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> WrappedMonoid Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> WrappedMonoid Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> WrappedMonoid Int -> Bool)
    describe "Option Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Option Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Option Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> Option Int -> Bool)
    describe "Arg Int Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Arg Int Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Arg Int Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> Arg Int Int -> Bool)
