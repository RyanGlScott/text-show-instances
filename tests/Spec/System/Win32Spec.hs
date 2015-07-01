{-# LANGUAGE CPP #-}
{-|
Module:      Spec.System.Win32Spec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for data types in the @Win32@ library.
-}
module Spec.System.Win32Spec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if defined(mingw32_HOST_OS)
import Instances.System.Win32 ()

import Spec.Utils (prop_matchesShow)

import System.Win32.DebugApi (DebugEventInfo, Exception)
import System.Win32.File (BY_HANDLE_FILE_INFORMATION, WIN32_FILE_ATTRIBUTE_DATA)
import System.Win32.Info (ProcessorArchitecture, SYSTEM_INFO)
import System.Win32.Time (FILETIME, SYSTEMTIME, TIME_ZONE_INFORMATION, TimeZoneId)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.System.Win32 ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if defined(mingw32_HOST_OS)
    describe "DebugEventInfo" $
        prop "Show instance" (prop_matchesShow :: Int -> DebugEventInfo -> Bool)
    describe "Exception" $
        prop "Show instance" (prop_matchesShow :: Int -> Exception -> Bool)
    describe "BY_HANDLE_FILE_INFORMATION" $
        prop "Show instance" (prop_matchesShow :: Int -> BY_HANDLE_FILE_INFORMATION -> Bool)
    describe "WIN32_FILE_ATTRIBUTE_DATA" $
        prop "Show instance" (prop_matchesShow :: Int -> WIN32_FILE_ATTRIBUTE_DATA -> Bool)
    describe "ProcessorArchitecture" $
        prop "Show instance" (prop_matchesShow :: Int -> ProcessorArchitecture -> Bool)
    describe "SYSTEM_INFO" $
        prop "Show instance" (prop_matchesShow :: Int -> SYSTEM_INFO -> Bool)
    describe "FILETIME" $
        prop "Show instance" (prop_matchesShow :: Int -> FILETIME -> Bool)
    describe "SYSTEMTIME" $
        prop "Show instance" (prop_matchesShow :: Int -> SYSTEMTIME -> Bool)
    describe "TIME_ZONE_INFORMATION" $
        prop "Show instance" (prop_matchesShow :: Int -> TIME_ZONE_INFORMATION -> Bool)
    describe "TimeZoneId" $
        prop "Show instance" (prop_matchesShow :: Int -> TimeZoneId -> Bool)
#else
    pure ()
#endif
