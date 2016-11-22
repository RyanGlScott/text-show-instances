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
import Data.Proxy (Proxy(..))

import Instances.System.Win32 ()

import Spec.Utils (matchesTextShowSpec)

import System.Win32.DebugApi (DebugEventInfo, Exception)
import System.Win32.File (BY_HANDLE_FILE_INFORMATION, WIN32_FILE_ATTRIBUTE_DATA)
import System.Win32.Info (ProcessorArchitecture, SYSTEM_INFO)
import System.Win32.Time (FILETIME, SYSTEMTIME, TIME_ZONE_INFORMATION, TimeZoneId)

import Test.Hspec (describe)

import TextShow.System.Win32 ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if defined(mingw32_HOST_OS)
    describe "DebugEventInfo" $
        matchesTextShowSpec (Proxy :: Proxy DebugEventInfo)
    describe "Exception" $
        matchesTextShowSpec (Proxy :: Proxy Exception)
    describe "BY_HANDLE_FILE_INFORMATION" $
        matchesTextShowSpec (Proxy :: Proxy BY_HANDLE_FILE_INFORMATION)
    describe "WIN32_FILE_ATTRIBUTE_DATA" $
        matchesTextShowSpec (Proxy :: Proxy WIN32_FILE_ATTRIBUTE_DATA)
    describe "ProcessorArchitecture" $
        matchesTextShowSpec (Proxy :: Proxy ProcessorArchitecture)
    describe "SYSTEM_INFO" $
        matchesTextShowSpec (Proxy :: Proxy SYSTEM_INFO)
    describe "FILETIME" $
        matchesTextShowSpec (Proxy :: Proxy FILETIME)
    describe "SYSTEMTIME" $
        matchesTextShowSpec (Proxy :: Proxy SYSTEMTIME)
    describe "TIME_ZONE_INFORMATION" $
        matchesTextShowSpec (Proxy :: Proxy TIME_ZONE_INFORMATION)
    describe "TimeZoneId" $
        matchesTextShowSpec (Proxy :: Proxy TimeZoneId)
#else
    pure ()
#endif
