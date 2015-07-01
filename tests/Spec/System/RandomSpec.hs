{-|
Module:      Spec.System.RandomSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for 'StdGen' values.
-}
module Spec.System.RandomSpec (main, spec) where

import Instances.System.Random ()

import Spec.Utils (prop_matchesShow)

import System.Random (StdGen)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.System.Random ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "StdGen" $
    prop "Show instance" (prop_matchesShow :: Int -> StdGen -> Bool)
