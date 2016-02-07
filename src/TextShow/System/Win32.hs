{-# LANGUAGE CPP             #-}

#if defined(mingw32_HOST_OS)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      TextShow.System.Win32
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @Win32@ library.
This module only exports functions if using Windows.

/Since: 2/
-}
module TextShow.System.Win32 (
#if !defined(mingw32_HOST_OS)
    ) where
#else
      showbDebugEventInfoPrec
    , showbExceptionPrec
    , showb_BY_HANDLE_FILE_INFORMATION_Prec
    , showb_WIN32_FILE_ATTRIBUTE_DATA_Prec
    , showbProcessorArchitecturePrec
    , showb_SYSTEM_INFO_Prec
    , showb_FILETIME_Prec
    , showb_SYSTEMTIME_Prec
    , showb_TIME_ZONE_INFORMATION_Prec
    , showbTimeZoneIdPrec
    ) where

import System.Win32.DebugApi (DebugEventInfo, Exception)
import System.Win32.File (BY_HANDLE_FILE_INFORMATION, WIN32_FILE_ATTRIBUTE_DATA)
import System.Win32.Info (ProcessorArchitecture, SYSTEM_INFO)
import System.Win32.Time (FILETIME, SYSTEMTIME, TIME_ZONE_INFORMATION, TimeZoneId)

import TextShow (TextShow(..), Builder)
import TextShow.TH (deriveTextShow)

-- | Convert a 'DebugEventInfo' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbDebugEventInfoPrec :: Int -> DebugEventInfo -> Builder
showbDebugEventInfoPrec = showbPrec
{-# INLINE showbDebugEventInfoPrec #-}

-- | Convert an 'Exception' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbExceptionPrec :: Int -> Exception -> Builder
showbExceptionPrec = showbPrec
{-# INLINE showbExceptionPrec #-}

-- | Convert a 'BY_HANDLE_FILE_INFORMATION' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showb_BY_HANDLE_FILE_INFORMATION_Prec :: Int -> BY_HANDLE_FILE_INFORMATION -> Builder
showb_BY_HANDLE_FILE_INFORMATION_Prec = showbPrec
{-# INLINE showb_BY_HANDLE_FILE_INFORMATION_Prec #-}

-- | Convert a 'WIN32_FILE_ATTRIBUTE_DATA' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showb_WIN32_FILE_ATTRIBUTE_DATA_Prec :: Int -> WIN32_FILE_ATTRIBUTE_DATA -> Builder
showb_WIN32_FILE_ATTRIBUTE_DATA_Prec = showbPrec
{-# INLINE showb_WIN32_FILE_ATTRIBUTE_DATA_Prec #-}

-- | Convert a 'ProcessorArchitecture' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbProcessorArchitecturePrec :: Int -> ProcessorArchitecture -> Builder
showbProcessorArchitecturePrec = showbPrec
{-# INLINE showbProcessorArchitecturePrec #-}

-- | Convert a 'SYSTEM_INFO' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showb_SYSTEM_INFO_Prec :: Int -> SYSTEM_INFO -> Builder
showb_SYSTEM_INFO_Prec = showbPrec
{-# INLINE showb_SYSTEM_INFO_Prec #-}

-- | Convert a 'FILETIME' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showb_FILETIME_Prec :: Int -> FILETIME -> Builder
showb_FILETIME_Prec = showbPrec
{-# INLINE showb_FILETIME_Prec #-}

-- | Convert a 'SYSTEMTIME' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showb_SYSTEMTIME_Prec :: Int -> SYSTEMTIME -> Builder
showb_SYSTEMTIME_Prec = showbPrec
{-# INLINE showb_SYSTEMTIME_Prec #-}

-- | Convert a 'TIME_ZONE_INFORMATION' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showb_TIME_ZONE_INFORMATION_Prec :: Int -> TIME_ZONE_INFORMATION -> Builder
showb_TIME_ZONE_INFORMATION_Prec = showbPrec
{-# INLINE showb_TIME_ZONE_INFORMATION_Prec #-}

-- | Convert a 'TimeZoneId' to a 'Builder'.
--
-- /Since: 2/
showbTimeZoneIdPrec :: TimeZoneId -> Builder
showbTimeZoneIdPrec = showb
{-# INLINE showbTimeZoneIdPrec #-}

$(deriveTextShow ''DebugEventInfo)
$(deriveTextShow ''Exception)
$(deriveTextShow ''BY_HANDLE_FILE_INFORMATION)
$(deriveTextShow ''WIN32_FILE_ATTRIBUTE_DATA)
$(deriveTextShow ''ProcessorArchitecture)
$(deriveTextShow ''SYSTEM_INFO)
$(deriveTextShow ''FILETIME)
$(deriveTextShow ''SYSTEMTIME)
$(deriveTextShow ''TIME_ZONE_INFORMATION)
$(deriveTextShow ''TimeZoneId)
#endif
