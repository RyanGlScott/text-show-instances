{-|
Module:      Spec.Data.TimeSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @time@ library.
-}
module Spec.Data.TimeSpec (main, spec) where

import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, UTCTime, NominalDiffTime)
import Data.Time.Clock.TAI (AbsoluteTime)
import Data.Time.LocalTime (TimeZone, TimeOfDay, LocalTime, ZonedTime)

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import TextShow.Data.Time ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Day" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Day -> Bool)
    describe "DiffTime" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DiffTime -> Bool)
    describe "UTCTime" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> UTCTime -> Bool)
    describe "NominalDiffTime" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> NominalDiffTime -> Bool)
    describe "AbsoluteTime" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> AbsoluteTime -> Bool)
    describe "TimeZone" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TimeZone -> Bool)
    describe "TimeOfDay" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TimeOfDay -> Bool)
    describe "LocalTime" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> LocalTime -> Bool)
    describe "ZonedTime" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ZonedTime -> Bool)
