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

import Spec.Utils (prop_matchesTextShow, prop_genericTextShow, prop_genericTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow.Data.Semigroup ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Min Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Min Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Min Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> Min Int -> Bool)
    describe "Max Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Max Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Max Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> Max Int -> Bool)
    describe "First Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> First Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> First Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> First Int -> Bool)
    describe "Last Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Last Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Last Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> Last Int -> Bool)
    describe "WrappedMonoid Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> WrappedMonoid Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> WrappedMonoid Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> WrappedMonoid Int -> Bool)
    describe "Option Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Option Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Option Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> Option Int -> Bool)
    describe "Arg Int Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Arg Int Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Arg Int Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> Arg Int Int -> Bool)
