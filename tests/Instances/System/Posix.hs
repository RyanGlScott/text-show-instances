{-# LANGUAGE CPP                #-}

#if !defined(mingw32_HOST_OS)
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

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

#if !defined(mingw32_HOST_OS)
import Instances.Miscellaneous ()

import Prelude ()
import Prelude.Compat

import System.Posix.DynamicLinker (RTLDFlags(..), DL(..))
import System.Posix.Process (ProcessStatus(..))
import System.Posix.User (GroupEntry(..), UserEntry(..))

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

deriving instance Bounded RTLDFlags
deriving instance Enum RTLDFlags
instance Arbitrary RTLDFlags where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary DL where
    arbitrary = oneof [pure Null, pure Next, pure Default, DLHandle <$> arbitrary]

instance Arbitrary ProcessStatus where
    arbitrary = oneof [ Exited     <$> arbitrary
# if MIN_VERSION_unix(2,7,0)
                      , Terminated <$> arbitrary <*> arbitrary
# else
                      , Terminated <$> arbitrary
# endif
                      , Stopped    <$> arbitrary
                      ]

instance Arbitrary GroupEntry where
    arbitrary = GroupEntry <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary UserEntry where
    arbitrary = UserEntry <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary <*> arbitrary
#endif
