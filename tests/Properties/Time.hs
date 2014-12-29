{-|
Module:      Properties.Time
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @time@ library.
-}
module Properties.Time (timeTests) where

import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, UTCTime, NominalDiffTime)
import Data.Time.Clock.TAI (AbsoluteTime)
import Data.Time.LocalTime (TimeZone, TimeOfDay, LocalTime, ZonedTime)

import Properties.Utils (prop_matchesShow)

import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Data.Time ()

timeTests :: [TestTree]
timeTests =
    [ testGroup "Text.Show.Text.Data.Time"
        [ testProperty "Day instance"                       (prop_matchesShow :: Int -> Day -> Bool)
        , testProperty "DiffTime instance"                  (prop_matchesShow :: Int -> DiffTime -> Bool)
        , testProperty "UTCTime instance"                   (prop_matchesShow :: Int -> UTCTime -> Bool)
        , testProperty "NominalDiffTime instance"           (prop_matchesShow :: Int -> NominalDiffTime -> Bool)
        , testProperty "AbsoluteTime instance"              (prop_matchesShow :: Int -> AbsoluteTime -> Bool)
        , testProperty "TimeZone instance"                  (prop_matchesShow :: Int -> TimeZone -> Bool)
        , testProperty "TimeOfDay instance"                 (prop_matchesShow :: Int -> TimeOfDay -> Bool)
        , testProperty "LocalTime instance"                 (prop_matchesShow :: Int -> LocalTime -> Bool)
        , testProperty "ZonedTime instance"                 (prop_matchesShow :: Int -> ZonedTime -> Bool)
        ]
    ]