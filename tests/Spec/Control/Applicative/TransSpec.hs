{-|
Module:      Spec.Control.Applicative.TransSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for applicative functor transformers.
-}
module Spec.Control.Applicative.TransSpec (main, spec) where

import Control.Applicative.Backwards (Backwards)
import Control.Applicative.Lift      (Lift)

import Instances.Control.Applicative.Trans ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.Control.Applicative.Trans ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Control.Applicative.Trans" $ do
    prop "Backwards Maybe Int instance" (prop_matchesShow :: Int -> Backwards Maybe Int -> Bool)
    prop "Lift Maybe Int instance"      (prop_matchesShow :: Int -> Lift Maybe Int -> Bool)
