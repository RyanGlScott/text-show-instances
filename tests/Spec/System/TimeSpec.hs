{-|
Module:      Spec.System.TimeSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in @old-time@.
-}
module Spec.System.TimeSpec (main, spec) where

import Spec.Utils (prop_matchesShow)

import System.Time (ClockTime, TimeDiff, CalendarTime, Month, Day)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import Text.Show.Text.System.Time ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.System.Time" $ do
    prop "ClockTime instance"    (prop_matchesShow :: Int -> ClockTime -> Bool)
    prop "TimeDiff instance"     (prop_matchesShow :: Int -> TimeDiff -> Bool)
    prop "CalendarTime instance" (prop_matchesShow :: Int -> CalendarTime -> Bool)
    prop "Month instance"        (prop_matchesShow :: Int -> Month -> Bool)
    prop "Day instance"          (prop_matchesShow :: Int -> Day -> Bool)
