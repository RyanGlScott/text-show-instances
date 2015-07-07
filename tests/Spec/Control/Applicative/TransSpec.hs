{-|
Module:      Spec.Control.Applicative.TransSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for applicative functor transformers.
-}
module Spec.Control.Applicative.TransSpec (main, spec) where

import Control.Applicative.Backwards (Backwards)
import Control.Applicative.Lift      (Lift)

import Instances.Control.Applicative.Trans ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow.Control.Applicative.Trans ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Backwards Maybe Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Backwards Maybe Int -> Bool)
    describe "Lift Maybe Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Lift Maybe Int -> Bool)
