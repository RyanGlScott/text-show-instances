{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.System.Posix
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @unix@ library.
-}
module Instances.System.Posix () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))

import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, nullPtr, plusPtr)

import System.Exit (ExitCode(..))
import System.Posix.DynamicLinker (RTLDFlags(..), DL(..))
import System.Posix.Process (ProcessStatus(..))
import System.Posix.Types (CGid(..), CUid(..))
import System.Posix.User (GroupEntry(..), UserEntry(..))

import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary RTLDFlags where
    arbitrary = oneof $ map pure [RTLD_LAZY, RTLD_NOW, RTLD_GLOBAL, RTLD_LOCAL]

instance Arbitrary DL where
    arbitrary = oneof [pure Null, pure Next, pure Default, DLHandle <$> arbitrary]

instance Arbitrary ProcessStatus where
    arbitrary = oneof [ Exited     <$> arbitrary
                      , Terminated <$> arbitrary <*> arbitrary
                      , Stopped    <$> arbitrary
                      ]

instance Arbitrary GroupEntry where
    arbitrary = GroupEntry <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary UserEntry where
    arbitrary = UserEntry <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary <*> arbitrary

-------------------------------------------------------------------------------
-- Miscellaneous Arbitrary instances (taken from text-show)
-------------------------------------------------------------------------------

instance Arbitrary ExitCode where
    arbitrary = oneof [pure ExitSuccess, ExitFailure <$> arbitrary]

instance Arbitrary (Ptr a) where
    arbitrary = plusPtr nullPtr <$> arbitrary

deriving instance Arbitrary CGid
deriving instance Arbitrary CInt
deriving instance Arbitrary CUid