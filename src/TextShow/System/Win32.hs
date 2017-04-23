{-# LANGUAGE CPP               #-}

#if defined(mingw32_HOST_OS)
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
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
# if MIN_VERSION_Win32(2,5,0)
import Data.Monoid.Compat ((<>))

import Graphics.Win32.GDI.AlphaBlend (BLENDFUNCTION)
import System.Win32.Automation.Input (HARDWAREINPUT, INPUT)
import System.Win32.Automation.Input.Key (KEYBDINPUT)
import System.Win32.Automation.Input.Mouse (MOUSEINPUT)
import System.Win32.Exception.Unsupported (Unsupported(..))
import System.Win32.Info.Version (ProductType, OSVERSIONINFOEX)
import System.Win32.Mem (MEMORY_BASIC_INFORMATION)
import System.Win32.SimpleMAPI (RecipientClass, Recipient, FileTag, Attachment, Message)

import TextShow (TextShow(..), fromString)
# endif

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

# if MIN_VERSION_Win32(2,5,0)
-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
$(deriveTextShow ''BLENDFUNCTION)
-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
$(deriveTextShow ''KEYBDINPUT)
-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
$(deriveTextShow ''MOUSEINPUT)
-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
$(deriveTextShow ''HARDWAREINPUT)
-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
$(deriveTextShow ''INPUT)
-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
$(deriveTextShow ''ProductType)
-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
$(deriveTextShow ''OSVERSIONINFOEX)
-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
$(deriveTextShow ''MEMORY_BASIC_INFORMATION)
-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
$(deriveTextShow ''RecipientClass)
-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
$(deriveTextShow ''Recipient)
-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
$(deriveTextShow ''FileTag)
-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
$(deriveTextShow ''Attachment)
-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
$(deriveTextShow ''Message)

-- | Only available with @Win32-2.5.0.0@ or later.
--
-- /Since: next/
instance TextShow Unsupported where
  showb (MissingLibrary  name reason)
    = "Can't load library \"" <> fromString name <> "\". "  <> fromString reason
  showb (MissingFunction name reason)
    = "Can't find \"" <> fromString name <> "\" function. " <> fromString reason
  showb (MissingValue    name reason)
    = "Can't use \""  <> fromString name <> "\" value. "    <> fromString reason
# endif
#endif
