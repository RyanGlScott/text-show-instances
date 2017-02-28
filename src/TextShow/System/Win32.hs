{-# LANGUAGE CPP             #-}

#if defined(mingw32_HOST_OS)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      TextShow.System.Win32
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @Win32@ library.
Only provided if using Windows.

/Since: 2/
-}
module TextShow.System.Win32 () where

#if defined(mingw32_HOST_OS)
import System.Win32.DebugApi (DebugEventInfo, Exception)
import System.Win32.File (BY_HANDLE_FILE_INFORMATION, WIN32_FILE_ATTRIBUTE_DATA)
import System.Win32.Info (ProcessorArchitecture, SYSTEM_INFO)
import System.Win32.Time (FILETIME, SYSTEMTIME, TIME_ZONE_INFORMATION, TimeZoneId)

import TextShow.TH (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''DebugEventInfo)
-- | /Since: 2/
$(deriveTextShow ''Exception)
-- | /Since: 2/
$(deriveTextShow ''BY_HANDLE_FILE_INFORMATION)
-- | /Since: 2/
$(deriveTextShow ''WIN32_FILE_ATTRIBUTE_DATA)
-- | /Since: 2/
$(deriveTextShow ''ProcessorArchitecture)
-- | /Since: 2/
$(deriveTextShow ''SYSTEM_INFO)
-- | /Since: 2/
$(deriveTextShow ''FILETIME)
-- | /Since: 2/
$(deriveTextShow ''SYSTEMTIME)
-- | /Since: 2/
$(deriveTextShow ''TIME_ZONE_INFORMATION)
-- | /Since: 2/
$(deriveTextShow ''TimeZoneId)
#endif
