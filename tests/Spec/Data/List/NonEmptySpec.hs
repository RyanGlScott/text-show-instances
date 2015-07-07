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

import Spec.Utils (prop_matchesTextShow, prop_genericTextShow, prop_genericTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow.Data.List.NonEmpty ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "NonEmpty Char" $ do
    prop "TextShow instance" (prop_matchesTextShow  :: Int -> NonEmpty Char -> Bool)
    prop "generic TextShow"  (prop_genericTextShow  :: Int -> NonEmpty Char -> Bool)
    prop "generic TextShow1" (prop_genericTextShow1 :: Int -> NonEmpty Char -> Bool)
