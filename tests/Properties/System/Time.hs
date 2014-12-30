{-|
Module:      Properties.System.Time
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in @old-time@.
-}
module Properties.System.Time (oldTimeTests) where

import Properties.Utils (prop_matchesShow)

import System.Time (ClockTime, TimeDiff, CalendarTime, Month, Day)

import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.System.Time ()

oldTimeTests :: [TestTree]
oldTimeTests =
    [ testGroup "Text.Show.Text.System.Time"
        [ testProperty "ClockTime instance"    (prop_matchesShow :: Int -> ClockTime -> Bool)
        , testProperty "TimeDiff instance"     (prop_matchesShow :: Int -> TimeDiff -> Bool)
        , testProperty "CalendarTime instance" (prop_matchesShow :: Int -> CalendarTime -> Bool)
        , testProperty "Month instance"        (prop_matchesShow :: Int -> Month -> Bool)
        , testProperty "Day instance"          (prop_matchesShow :: Int -> Day -> Bool)
        ]
    ]
