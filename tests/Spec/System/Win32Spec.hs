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
    describe "Text.Show.Text.System.Win32" $ do
        prop "DebugEventInfo instance"             (prop_matchesShow :: Int -> DebugEventInfo -> Bool)
        prop "Exception instance"                  (prop_matchesShow :: Int -> Exception -> Bool)
        prop "BY_HANDLE_FILE_INFORMATION instance" (prop_matchesShow :: Int -> BY_HANDLE_FILE_INFORMATION -> Bool)
        prop "WIN32_FILE_ATTRIBUTE_DATA instance"  (prop_matchesShow :: Int -> WIN32_FILE_ATTRIBUTE_DATA -> Bool)
        prop "ProcessorArchitecture instance"      (prop_matchesShow :: Int -> ProcessorArchitecture -> Bool)
        prop "SYSTEM_INFO instance"                (prop_matchesShow :: Int -> SYSTEM_INFO -> Bool)
        prop "FILETIME instance"                   (prop_matchesShow :: Int -> FILETIME -> Bool)
        prop "SYSTEMTIME instance"                 (prop_matchesShow :: Int -> SYSTEMTIME -> Bool)
        prop "TIME_ZONE_INFORMATION instance"      (prop_matchesShow :: Int -> TIME_ZONE_INFORMATION -> Bool)
        prop "TimeZoneId instance"                 (prop_matchesShow :: Int -> TimeZoneId -> Bool)
#else
    pure ()
#endif
