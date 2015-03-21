{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.System.Win32
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Win32@ library.
-}
module Instances.System.Win32 () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)

import Data.Functor ((<$>))
#endif

import Instances.Miscellaneous ()

import System.Win32.DebugApi (DebugEventInfo(..), Exception(..))
import System.Win32.File (BY_HANDLE_FILE_INFORMATION(..), WIN32_FILE_ATTRIBUTE_DATA(..))
import System.Win32.Info (ProcessorArchitecture(..), SYSTEM_INFO(..))
import System.Win32.Time (FILETIME(..), SYSTEMTIME(..),
                          TIME_ZONE_INFORMATION(..), TimeZoneId(..))

import Test.Tasty.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

instance Arbitrary DebugEventInfo where
    arbitrary = oneof [ pure UnknownDebugEvent
                      , Exception     <$> arbitrary <*> arbitrary
                      , CreateThread  <$> arbitrary
                      , CreateProcess <$> arbitrary <*> arbitrary <*> arbitrary
                      , ExitThread    <$> arbitrary
                      , ExitProcess   <$> arbitrary
                      , LoadDll       <$> arbitrary
                      , UnloadDll     <$> arbitrary
                      , DebugString   <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

instance Arbitrary Exception where
    arbitrary = oneof [ pure UnknownException
                      , AccessViolation <$> arbitrary <*> arbitrary
                      , pure ArrayBoundsExceeded
                      , pure Breakpoint
                      , pure DataTypeMisalignment
                      , pure FltDenormalOperand
                      , pure FltDivideByZero
                      , pure FltInexactResult
                      , pure FltInvalidOperation
                      , pure FltOverflow
                      , pure FltStackCheck
                      , pure FltUnderflow
                      , pure IllegalInstruction
                      , pure InPageError
                      , pure IntDivideByZero
                      , pure IntOverflow
                      , pure InvalidDisposition
                      , pure NonContinuable
                      , pure PrivilegedInstruction
                      , pure SingleStep
                      , pure StackOverflow
                      ]

instance Arbitrary BY_HANDLE_FILE_INFORMATION where
    arbitrary = BY_HANDLE_FILE_INFORMATION <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary WIN32_FILE_ATTRIBUTE_DATA where
    arbitrary = WIN32_FILE_ATTRIBUTE_DATA <$> arbitrary <*> arbitrary <*> arbitrary
                                          <*> arbitrary <*> arbitrary

instance Arbitrary ProcessorArchitecture where
    arbitrary = oneof [ PaUnknown <$> arbitrary
                      , pure PaIntel
                      , pure PaMips
                      , pure PaAlpha
                      , pure PaPpc
                      , pure PaIa64
                      , pure PaIa32OnIa64
                      , pure PaAmd64
                      ]

instance Arbitrary SYSTEM_INFO where
    arbitrary = SYSTEM_INFO <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary

deriving instance Arbitrary FILETIME

instance Arbitrary SYSTEMTIME where
    arbitrary = SYSTEMTIME <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TIME_ZONE_INFORMATION where
    arbitrary = TIME_ZONE_INFORMATION <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                                      <*> arbitrary <*> arbitrary <*> arbitrary

deriving instance Bounded TimeZoneId
deriving instance Enum TimeZoneId
instance Arbitrary TimeZoneId where
    arbitrary = arbitraryBoundedEnum
