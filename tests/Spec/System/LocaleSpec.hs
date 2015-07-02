{-|
Module:      Spec.System.LocaleSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for old 'TimeLocale's.
-}
module Spec.System.LocaleSpec (main, spec) where

import Instances.System.Locale ()

import Spec.Utils (prop_matchesShow)

import System.Locale (TimeLocale)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.System.Locale ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "TimeLocale" $
    prop "Show instance" (prop_matchesShow :: Int -> TimeLocale -> Bool)
