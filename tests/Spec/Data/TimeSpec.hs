{-|
Module:      Spec.Data.TimeSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for data types in the @time@ library.
-}
module Spec.Data.TimeSpec (main, spec) where

import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, UTCTime, NominalDiffTime)
import Data.Time.Clock.TAI (AbsoluteTime)
import Data.Time.LocalTime (TimeZone, TimeOfDay, LocalTime, ZonedTime)

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import Text.Show.Text.Data.Time ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Time" $ do
    prop "Day instance"             (prop_matchesShow :: Int -> Day -> Bool)
    prop "DiffTime instance"        (prop_matchesShow :: Int -> DiffTime -> Bool)
    prop "UTCTime instance"         (prop_matchesShow :: Int -> UTCTime -> Bool)
    prop "NominalDiffTime instance" (prop_matchesShow :: Int -> NominalDiffTime -> Bool)
    prop "AbsoluteTime instance"    (prop_matchesShow :: Int -> AbsoluteTime -> Bool)
    prop "TimeZone instance"        (prop_matchesShow :: Int -> TimeZone -> Bool)
    prop "TimeOfDay instance"       (prop_matchesShow :: Int -> TimeOfDay -> Bool)
    prop "LocalTime instance"       (prop_matchesShow :: Int -> LocalTime -> Bool)
    prop "ZonedTime instance"       (prop_matchesShow :: Int -> ZonedTime -> Bool)
