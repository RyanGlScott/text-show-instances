{-|
Module:      Properties.Control.Applicative.Trans
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for applicative functor transformers.
-}
module Properties.Control.Applicative.Trans (applicativeFunctorTransformerTests) where

import Control.Applicative.Backwards (Backwards)
import Control.Applicative.Lift      (Lift)

import Instances.Control.Applicative.Trans ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Control.Applicative.Trans ()

applicativeFunctorTransformerTests :: [TestTree]
applicativeFunctorTransformerTests =
    [ testGroup "Text.Show.Text.Control.Applicative.Trans"
        [ testProperty "Backwards Maybe Int instance" (prop_matchesShow :: Int -> Backwards Maybe Int -> Bool)
        , testProperty "Lift Maybe Int instance"      (prop_matchesShow :: Int -> Lift Maybe Int -> Bool)
        ]
    ]