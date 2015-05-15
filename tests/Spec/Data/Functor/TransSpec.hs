{-|
Module:      Spec.Data.Functor.TransSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for functor transformers.
-}
module Spec.Data.Functor.TransSpec (main, spec) where

import Data.Functor.Compose  (Compose)
import Data.Functor.Constant (Constant)
import Data.Functor.Product  (Product)
import Data.Functor.Reverse  (Reverse)
import Data.Functor.Sum      (Sum)

import Instances.Data.Functor.Trans ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.Data.Functor.Trans ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Functor.Trans" $ do
    prop "Compose Maybe [] Char instance" (prop_matchesShow :: Int -> Compose Maybe [] Char -> Bool)
    prop "Constant Int Char instance"     (prop_matchesShow :: Int -> Constant Int Char -> Bool)
    prop "Product Maybe [] Char instance" (prop_matchesShow :: Int -> Product Maybe [] Char -> Bool)
    prop "Reverse Maybe Int instance"     (prop_matchesShow :: Int -> Reverse Maybe Int -> Bool)
    prop "Sum Maybe [] Char instance"     (prop_matchesShow :: Int -> Sum Maybe [] Char -> Bool)
