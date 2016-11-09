{-# LANGUAGE CPP                #-}

#if defined(MIN_VERSION_ghc_boot)
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.GHC.PackageDb
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the "GHC.PackageDb" module.
-}
module Instances.GHC.PackageDb () where

#if defined(MIN_VERSION_ghc_boot)
import GHC.Generics
import GHC.PackageDb

import Instances.Miscellaneous ()
import Instances.Utils ((<@>))

import Test.QuickCheck (Arbitrary(..), genericArbitrary)

instance ( Arbitrary srcpkgid
         , Arbitrary srcpkgname
         , Arbitrary unitid
         , Arbitrary modulename
# if __GLASGOW_HASKELL__ >= 801
         , Arbitrary mod
# endif
         )
  => Arbitrary ( InstalledPackageInfo srcpkgid srcpkgname unitid modulename
# if __GLASGOW_HASKELL__ >= 801
                 mod
# endif
               ) where
    arbitrary = InstalledPackageInfo <$> arbitrary <*> arbitrary <*> arbitrary
                                     <*> arbitrary <*> arbitrary <@> []
                                     <*> arbitrary <@> []        <@> []
                                     <@> []        <@> []        <@> []
                                     <@> []        <@> []        <@> []
                                     <@> []        <@> []        <@> []
                                     <@> []        <*> arbitrary <*> arbitrary
                                     <*> arbitrary <*> arbitrary

# if __GLASGOW_HASKELL__ >= 801
deriving instance Generic (DbModule unitid modulename)
instance (Arbitrary unitid, Arbitrary modulename)
  => Arbitrary (DbModule unitid modulename) where
    arbitrary = genericArbitrary
# else
deriving instance Generic (ExposedModule unitid modulename)
instance (Arbitrary unitid, Arbitrary modulename)
  => Arbitrary (ExposedModule unitid modulename) where
    arbitrary = genericArbitrary

deriving instance Generic (OriginalModule unitid modulename)
instance (Arbitrary unitid, Arbitrary modulename)
  => Arbitrary (OriginalModule unitid modulename) where
    arbitrary = genericArbitrary
# endif
#endif
