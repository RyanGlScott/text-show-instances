{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Miscellaneous
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides miscellaneous 'Arbitrary' instances (taken from @text-show@).
-}
module Instances.Miscellaneous () where

#include "HsBaseConfig.h"

import Foreign.Ptr (Ptr, nullPtr, plusPtr)

import Generics.Deriving.Instances ()

import Instances.Utils.GenericArbitrary (genericArbitrary)

import Prelude ()
import Prelude.Compat

import System.Exit (ExitCode(..))
import System.IO (Handle, stdin, stdout, stderr)

import Test.QuickCheck (Arbitrary(..), oneof)

#if MIN_VERSION_base(4,5,0)

import Foreign.C.Types (CInt(..), CUIntPtr(..))
# if defined(HTYPE_GID_T)
import System.Posix.Types (CGid(..))
# endif
# if defined(HTYPE_UID_T)
import System.Posix.Types (CUid(..))
# endif

#else

import Foreign.C.Types (CInt, CUIntPtr)
# if defined(HTYPE_GID_T)
import System.Posix.Types (CGid)
# endif
# if defined(HTYPE_UID_T)
import System.Posix.Types (CUid)
# endif
import Test.QuickCheck (arbitrarySizedBoundedIntegral)

#endif

instance Arbitrary ExitCode where
    arbitrary = genericArbitrary

instance Arbitrary Handle where
    arbitrary = oneof $ map pure [stdin, stdout, stderr]

instance Arbitrary (Ptr a) where
    arbitrary = plusPtr nullPtr <$> arbitrary

#if MIN_VERSION_base(4,5,0)

deriving instance Arbitrary CInt
deriving instance Arbitrary CUIntPtr

# if defined(HTYPE_GID_T)
deriving instance Arbitrary CGid
# endif

# if defined(HTYPE_UID_T)
deriving instance Arbitrary CUid
# endif

#else

instance Arbitrary CInt where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CUIntPtr where
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
