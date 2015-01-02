{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Miscellaneous
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides miscellaneous 'Arbitrary' instances (taken from @text-show@).
-}
module Instances.Miscellaneous () where

#include "HsBaseConfig.h"

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (pure)
#endif

import Data.Functor ((<$>))

import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, nullPtr, plusPtr)

import System.Exit (ExitCode(..))
#if defined(HTYPE_GID_T)
import System.Posix.Types (CGid(..))
#endif
#if defined(HTYPE_UID_T)
import System.Posix.Types (CUid(..))
#endif

import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary ExitCode where
    arbitrary = oneof [pure ExitSuccess, ExitFailure <$> arbitrary]

instance Arbitrary (Ptr a) where
    arbitrary = plusPtr nullPtr <$> arbitrary

deriving instance Arbitrary CInt
#if defined(HTYPE_GID_T)
deriving instance Arbitrary CGid
#endif
#if defined(HTYPE_UID_T)
deriving instance Arbitrary CUid
#endif