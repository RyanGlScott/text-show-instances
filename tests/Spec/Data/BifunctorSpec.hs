{-|
Module:      Spec.Data.BifunctorSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @bifunctors@ library.
-}
module Spec.Data.BifunctorSpec (main, spec) where

import Data.Bifunctor.Biff (Biff)
import Data.Bifunctor.Clown (Clown)
import Data.Bifunctor.Fix (Fix)
import Data.Bifunctor.Flip (Flip)
import Data.Bifunctor.Join (Join)
import Data.Bifunctor.Joker (Joker)
import Data.Bifunctor.Product (Product)
import Data.Bifunctor.Sum (Sum)
import Data.Bifunctor.Tannen (Tannen)
import Data.Bifunctor.Wrapped (WrappedBifunctor)

import Instances.Data.Bifunctor ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow.Data.Bifunctor ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Biff Either [] Maybe Char Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Biff Either [] Maybe Char Int -> Bool)
    describe "Clown [] Char Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Clown [] Char Int -> Bool)
    describe "Fix Either Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Fix Either Int -> Bool)
    describe "Flip Either Int Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Flip Either Int Char -> Bool)
    describe "Join Either Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Join Either Int -> Bool)
    describe "Joker [] Char Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Joker [] Char Int -> Bool)
    describe "Product Either ((,,) Bool) Int Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Product Either ((,,) Bool) Int Char -> Bool)
    describe "Sum Either ((,,) Bool) Int Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Sum Either ((,,) Bool) Int Char -> Bool)
    describe "Tannen Maybe Either Int Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Tannen Maybe Either Int Char -> Bool)
    describe "WrappedBifunctor Either Int Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> WrappedBifunctor Either Int Char -> Bool)
