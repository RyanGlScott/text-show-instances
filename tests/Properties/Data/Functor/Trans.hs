{-|
Module:      Properties.Data.Functor.Trans
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for functor transformers.
-}
module Properties.Data.Functor.Trans (functorTransformerTests) where

import Data.Functor.Compose  (Compose)
import Data.Functor.Constant (Constant)
import Data.Functor.Product  (Product)
import Data.Functor.Reverse  (Reverse)
import Data.Functor.Sum      (Sum)

import Instances.Data.Functor.Trans ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Data.Functor.Trans ()

functorTransformerTests :: [TestTree]
functorTransformerTests =
    [ testGroup "Text.Show.Text.Data.Functor.Trans"
        [ testProperty "Compose Maybe [] Char instance" (prop_matchesShow :: Int -> Compose Maybe [] Char -> Bool)
        , testProperty "Constant Int Char instance"     (prop_matchesShow :: Int -> Constant Int Char -> Bool)
        , testProperty "Product Maybe [] Char instance" (prop_matchesShow :: Int -> Product Maybe [] Char -> Bool)
        , testProperty "Reverse Maybe Int instance"     (prop_matchesShow :: Int -> Reverse Maybe Int -> Bool)
        , testProperty "Sum Maybe [] Char instance"     (prop_matchesShow :: Int -> Sum Maybe [] Char -> Bool)
        ]
    ]