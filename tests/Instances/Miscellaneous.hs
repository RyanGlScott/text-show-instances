{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Miscellaneous
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides miscellaneous 'Arbitrary' instances (taken from @text-show@).
-}
module Instances.Miscellaneous () where

#include "HsBaseConfig.h"

import Data.Version (Version(..))

import Foreign.Ptr (Ptr, nullPtr, plusPtr)

import Prelude ()
import Prelude.Compat

import System.Exit (ExitCode(..))

import Test.QuickCheck (Arbitrary(..), oneof)

#if MIN_VERSION_base(4,5,0)

import Foreign.C.Types (CInt(..))
# if defined(HTYPE_GID_T)
import System.Posix.Types (CGid(..))
# endif
# if defined(HTYPE_UID_T)
import System.Posix.Types (CUid(..))
# endif

#else

import Foreign.C.Types (CInt)
# if defined(HTYPE_GID_T)
import System.Posix.Types (CGid)
# endif
# if defined(HTYPE_UID_T)
import System.Posix.Types (CUid)
# endif
import Test.QuickCheck (arbitrarySizedBoundedIntegral)

#endif

instance Arbitrary ExitCode where
    arbitrary = oneof [pure ExitSuccess, ExitFailure <$> arbitrary]

instance Arbitrary (Ptr a) where
    arbitrary = plusPtr nullPtr <$> arbitrary

instance Arbitrary Version where
    arbitrary = pure $ Version [0] [""]
--     arbitrary = Version <$> arbitrary <*> arbitrary

#if MIN_VERSION_base(4,5,0)

deriving instance Arbitrary CInt

# if defined(HTYPE_GID_T)
deriving instance Arbitrary CGid
# endif

# if defined(HTYPE_UID_T)
deriving instance Arbitrary CUid
# endif

#else

instance Arbitrary CInt where
    arbitrary = arbitrarySizedBoundedIntegral

# if defined(HTYPE_GID_T)
instance Arbitrary CGid where
    arbitrary = arbitrarySizedBoundedIntegral
# endif

# if defined(HTYPE_UID_T)
instance Arbitrary CUid where
    arbitrary = arbitrarySizedBoundedIntegral
# endif

#endif