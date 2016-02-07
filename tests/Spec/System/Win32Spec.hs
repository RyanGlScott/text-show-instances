{-# LANGUAGE CPP #-}
{-|
Module:      Spec.System.Win32Spec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @Win32@ library.
-}
module Spec.System.Win32Spec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if defined(mingw32_HOST_OS)
import Instances.System.Win32 ()

import Spec.Utils (prop_matchesTextShow)

import System.Win32.DebugApi (DebugEventInfo, Exception)
import System.Win32.File (BY_HANDLE_FILE_INFORMATION, WIN32_FILE_ATTRIBUTE_DATA)
import System.Win32.Info (ProcessorArchitecture, SYSTEM_INFO)
import System.Win32.Time (FILETIME, SYSTEMTIME, TIME_ZONE_INFORMATION, TimeZoneId)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)

import TextShow.System.Win32 ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if defined(mingw32_HOST_OS)
    describe "DebugEventInfo" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DebugEventInfo -> Bool)
    describe "Exception" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Exception -> Bool)
    describe "BY_HANDLE_FILE_INFORMATION" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> BY_HANDLE_FILE_INFORMATION -> Bool)
    describe "WIN32_FILE_ATTRIBUTE_DATA" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> WIN32_FILE_ATTRIBUTE_DATA -> Bool)
    describe "ProcessorArchitecture" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ProcessorArchitecture -> Bool)
    describe "SYSTEM_INFO" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> SYSTEM_INFO -> Bool)
    describe "FILETIME" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> FILETIME -> Bool)
    describe "SYSTEMTIME" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> SYSTEMTIME -> Bool)
    describe "TIME_ZONE_INFORMATION" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TIME_ZONE_INFORMATION -> Bool)
    describe "TimeZoneId" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TimeZoneId -> Bool)
#else
    pure ()
#endif
