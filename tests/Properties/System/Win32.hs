{-|
Module:      Properties.System.Win32
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Win32@ library.
-}
module Properties.System.Win32 (win32Tests) where

import Instances.System.Win32 ()

import Properties.Utils (prop_matchesShow)

import System.Win32.DebugApi (DebugEventInfo, Exception)
import System.Win32.File (BY_HANDLE_FILE_INFORMATION, WIN32_FILE_ATTRIBUTE_DATA)
import System.Win32.Info (ProcessorArchitecture, SYSTEM_INFO)
import System.Win32.Time (FILETIME, SYSTEMTIME, TIME_ZONE_INFORMATION, TimeZoneId)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.System.Win32 ()

win32Tests :: [TestTree]
win32Tests =
    [ testGroup "Text.Show.Text.System.Win32"
        [ testProperty "DebugEventInfo instance"             (prop_matchesShow :: Int -> DebugEventInfo -> Bool)
        , testProperty "Exception instance"                  (prop_matchesShow :: Int -> Exception -> Bool)
        , testProperty "BY_HANDLE_FILE_INFORMATION instance" (prop_matchesShow :: Int -> BY_HANDLE_FILE_INFORMATION -> Bool)
        , testProperty "WIN32_FILE_ATTRIBUTE_DATA instance"  (prop_matchesShow :: Int -> WIN32_FILE_ATTRIBUTE_DATA -> Bool)
        , testProperty "ProcessorArchitecture instance"      (prop_matchesShow :: Int -> ProcessorArchitecture -> Bool)
        , testProperty "SYSTEM_INFO instance"                (prop_matchesShow :: Int -> SYSTEM_INFO -> Bool)
        , testProperty "FILETIME instance"                   (prop_matchesShow :: Int -> FILETIME -> Bool)
        , testProperty "SYSTEMTIME instance"                 (prop_matchesShow :: Int -> SYSTEMTIME -> Bool)
        , testProperty "TIME_ZONE_INFORMATION instance"      (prop_matchesShow :: Int -> TIME_ZONE_INFORMATION -> Bool)
        , testProperty "TimeZoneId instance"                 (prop_matchesShow :: Int -> TimeZoneId -> Bool)
        ]
    ]