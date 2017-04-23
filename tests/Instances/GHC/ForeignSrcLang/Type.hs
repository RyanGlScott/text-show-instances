{-# LANGUAGE CPP                #-}

#if defined(MIN_VERSION_ghc_boot_th) && MIN_VERSION_ghc_boot_th(8,2,0)
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.GHC.ForeignSrcLang.Type
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for the 'ForeignSrcLang' data type.
-}
module Instances.GHC.ForeignSrcLang.Type () where

#if defined(MIN_VERSION_ghc_boot_th) && MIN_VERSION_ghc_boot_th(8,2,0)
import GHC.ForeignSrcLang.Type (ForeignSrcLang(..))
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

deriving instance Bounded ForeignSrcLang
deriving instance Enum ForeignSrcLang
instance Arbitrary ForeignSrcLang where
    arbitrary = arbitraryBoundedEnum
#endif
