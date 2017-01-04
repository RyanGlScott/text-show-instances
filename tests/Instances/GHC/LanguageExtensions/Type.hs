{-# LANGUAGE CPP                #-}

#if defined(MIN_VERSION_ghc_boot_th)
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.GHC.LanguageExtensions.Type
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for the 'Extension' data type.
-}
module Instances.GHC.LanguageExtensions.Type () where

#if defined(MIN_VERSION_ghc_boot_th)
import GHC.LanguageExtensions.Type (Extension(..))
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

deriving instance Bounded Extension
instance Arbitrary Extension where
    arbitrary = arbitraryBoundedEnum
#endif
