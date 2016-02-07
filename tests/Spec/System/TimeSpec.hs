{-|
Module:      Spec.System.TimeSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@QuickCheck@ properties for data types in @old-time@.
-}
module Spec.System.TimeSpec (main, spec) where

import Spec.Utils (prop_matchesTextShow)

import System.Time (ClockTime, TimeDiff, CalendarTime, Month, Day)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import TextShow.System.Time ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "ClockTime" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ClockTime -> Bool)
    describe "TimeDiff" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TimeDiff -> Bool)
    describe "CalendarTime" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CalendarTime -> Bool)
    describe "Month" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Month -> Bool)
    describe "Day" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Day -> Bool)
