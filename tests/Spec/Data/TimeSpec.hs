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
spec = parallel $ do
    describe "Day" $
        prop "Show instance" (prop_matchesShow :: Int -> Day -> Bool)
    describe "DiffTime" $
        prop "Show instance" (prop_matchesShow :: Int -> DiffTime -> Bool)
    describe "UTCTime" $
        prop "Show instance" (prop_matchesShow :: Int -> UTCTime -> Bool)
    describe "NominalDiffTime" $
        prop "Show instance" (prop_matchesShow :: Int -> NominalDiffTime -> Bool)
    describe "AbsoluteTime" $
        prop "Show instance" (prop_matchesShow :: Int -> AbsoluteTime -> Bool)
    describe "TimeZone" $
        prop "Show instance" (prop_matchesShow :: Int -> TimeZone -> Bool)
    describe "TimeOfDay" $
        prop "Show instance" (prop_matchesShow :: Int -> TimeOfDay -> Bool)
    describe "LocalTime" $
        prop "Show instance" (prop_matchesShow :: Int -> LocalTime -> Bool)
    describe "ZonedTime" $
        prop "Show instance" (prop_matchesShow :: Int -> ZonedTime -> Bool)
