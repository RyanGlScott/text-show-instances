{-# LANGUAGE CPP                        #-}

#if defined(mingw32_HOST_OS)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

# if __GLASGOW_HASKELL __ >= 702
{-# LANGUAGE DeriveGeneric              #-}
# endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.System.Win32
Copyright:   (C) 2014-2016 Ryan Scott
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

# if __GLASGOW_HASKELL__ >= 704
deriving instance Generic DebugEventInfo
deriving instance Generic Exception
deriving instance Generic BY_HANDLE_FILE_INFORMATION
deriving instance Generic WIN32_FILE_INFORMATION
deriving instance Generic ProcessorArchitecture
deriving instance Generic SYSTEM_INFO
deriving instance Generic SYSTEMTIME
deriving instance Generic TIME_ZONE_INFORMATION
# else
$(Generics.deriveAll0 ''DebugEventInfo)
$(Generics.deriveAll0 ''Exception)
$(Generics.deriveAll0 ''BY_HANDLE_FILE_INFORMATION)
$(Generics.deriveAll0 ''WIN32_FILE_INFORMATION)
$(Generics.deriveAll0 ''ProcessorArchitecture)
$(Generics.deriveAll0 ''SYSTEM_INFO)
$(Generics.deriveAll0 ''SYSTEMTIME)
$(Generics.deriveAll0 ''TIME_ZONE_INFORMATION)
# endif
#endif
