{-# LANGUAGE CPP                        #-}

#if defined(mingw32_HOST_OS)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

# if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
# endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.System.Win32
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Win32@ library.
-}
module Instances.System.Win32 () where

#if defined(mingw32_HOST_OS)
# if __GLASGOW_HASKELL__ >= 704
import           GHC.Generics (Generic)
# else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
# endif

import           Instances.Miscellaneous ()
import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           System.Win32.DebugApi (DebugEventInfo(..), Exception(..))
import           System.Win32.File (BY_HANDLE_FILE_INFORMATION(..), WIN32_FILE_ATTRIBUTE_DATA(..))
import           System.Win32.Info (ProcessorArchitecture(..), SYSTEM_INFO(..))
import           System.Win32.Time (FILETIME(..), SYSTEMTIME(..),
                                    TIME_ZONE_INFORMATION(..), TimeZoneId(..))

#if MIN_VERSION_Win32(2,5,0)
import           Graphics.Win32.GDI.AlphaBlend (BLENDFUNCTION(..))
import           System.Win32.Automation.Input (HARDWAREINPUT(..), INPUT(..))
import           System.Win32.Automation.Input.Key (KEYBDINPUT(..))
import           System.Win32.Automation.Input.Mouse (MOUSEINPUT(..))
import           System.Win32.Exception.Unsupported (Unsupported(..))
import           System.Win32.Info.Version (ProductType(..), OSVERSIONINFOEX(..))
import           System.Win32.Mem (MEMORY_BASIC_INFORMATION(..))
import           System.Win32.SimpleMAPI (RecipientClass(..), Recipient(..), FileTag(..),
                                          Attachment(..), Message(..))
#endif

import           Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance Arbitrary DebugEventInfo where
    arbitrary = genericArbitrary

instance Arbitrary Exception where
    arbitrary = genericArbitrary

instance Arbitrary BY_HANDLE_FILE_INFORMATION where
    arbitrary = genericArbitrary

instance Arbitrary WIN32_FILE_ATTRIBUTE_DATA where
    arbitrary = genericArbitrary

instance Arbitrary ProcessorArchitecture where
    arbitrary = genericArbitrary

instance Arbitrary SYSTEM_INFO where
    arbitrary = genericArbitrary

deriving instance Arbitrary FILETIME

instance Arbitrary SYSTEMTIME where
    arbitrary = genericArbitrary

instance Arbitrary TIME_ZONE_INFORMATION where
    arbitrary = genericArbitrary

deriving instance Bounded TimeZoneId
deriving instance Enum TimeZoneId
instance Arbitrary TimeZoneId where
    arbitrary = arbitraryBoundedEnum

#if MIN_VERSION_Win32(2,5,0)
instance Arbitrary BLENDFUNCTION where
    arbitrary = genericArbitrary

instance Arbitrary KEYBDINPUT where
    arbitrary = genericArbitrary

instance Arbitrary MOUSEINPUT where
    arbitrary = genericArbitrary

instance Arbitrary HARDWAREINPUT where
    arbitrary = genericArbitrary

instance Arbitrary INPUT where
    arbitrary = genericArbitrary

instance Arbitrary ProductType where
    arbitrary = genericArbitrary

instance Arbitrary OSVERSIONINFOEX where
    arbitrary = genericArbitrary

instance Arbitrary MEMORY_BASIC_INFORMATION where
    arbitrary = genericArbitrary

deriving instance Bounded RecipientClass
instance Arbitrary RecipientClass where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Recipient where
    arbitrary = genericArbitrary

instance Arbitrary FileTag where
    arbitrary = genericArbitrary

instance Arbitrary Attachment where
    arbitrary = genericArbitrary

instance Arbitrary Message where
    arbitrary = genericArbitrary

instance Arbitrary Unsupported where
    arbitrary = genericArbitrary
#endif

# if __GLASGOW_HASKELL__ >= 704
deriving instance Generic DebugEventInfo
deriving instance Generic Exception
deriving instance Generic BY_HANDLE_FILE_INFORMATION
deriving instance Generic WIN32_FILE_ATTRIBUTE_DATA
deriving instance Generic ProcessorArchitecture
deriving instance Generic SYSTEM_INFO
deriving instance Generic SYSTEMTIME
deriving instance Generic TIME_ZONE_INFORMATION
#  if MIN_VERSION_Win32(2,5,0)
deriving instance Generic BLENDFUNCTION
deriving instance Generic KEYBDINPUT
deriving instance Generic MOUSEINPUT
deriving instance Generic HARDWAREINPUT
deriving instance Generic INPUT
deriving instance Generic ProductType
deriving instance Generic OSVERSIONINFOEX
deriving instance Generic MEMORY_BASIC_INFORMATION
deriving instance Generic RecipientClass
deriving instance Generic Recipient
deriving instance Generic FileTag
deriving instance Generic Attachment
deriving instance Generic Message
deriving instance Generic Unsupported
#  endif
# else
$(Generics.deriveAll0 ''DebugEventInfo)
$(Generics.deriveAll0 ''Exception)
$(Generics.deriveAll0 ''BY_HANDLE_FILE_INFORMATION)
$(Generics.deriveAll0 ''WIN32_FILE_ATTRIBUTE_DATA)
$(Generics.deriveAll0 ''ProcessorArchitecture)
$(Generics.deriveAll0 ''SYSTEM_INFO)
$(Generics.deriveAll0 ''SYSTEMTIME)
$(Generics.deriveAll0 ''TIME_ZONE_INFORMATION)
# endif
#endif
