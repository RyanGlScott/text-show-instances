{-|
Module:      Spec.Data.List.NonEmptySpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'NonEmpty' lists.
-}
module Spec.Data.List.NonEmptySpec (main, spec) where

import Data.List.NonEmpty (NonEmpty)

import Instances.Data.List.NonEmpty ()

import Spec.Utils (prop_matchesShow, prop_genericShow, prop_genericShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.Data.List.NonEmpty ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "NonEmpty Char" $ do
    prop "Show instance" (prop_matchesShow  :: Int -> NonEmpty Char -> Bool)
    prop "generic Show"  (prop_genericShow  :: Int -> NonEmpty Char -> Bool)
    prop "generic Show1" (prop_genericShow1 :: Int -> NonEmpty Char -> Bool)
