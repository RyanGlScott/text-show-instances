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
import Instances.Utils.GenericArbitrary (genericArbitrary)

import Test.QuickCheck (Arbitrary(..))

instance ( Arbitrary compid
         , Arbitrary srcpkgid
         , Arbitrary srcpkgname
         , Arbitrary instunitid
# if MIN_VERSION_ghc_boot(8,1,0)
         , Arbitrary unitid
         , Arbitrary modulename
         , Arbitrary mod
# endif
         )
  => Arbitrary ( InstalledPackageInfo compid srcpkgid srcpkgname instunitid
# if MIN_VERSION_ghc_boot(8,1,0)
                 unitid modulename mod
# endif
               ) where
    arbitrary = pure InstalledPackageInfo
#if MIN_VERSION_ghc_boot(8,1,0)
                     <*> arbitrary <*> arbitrary <*> arbitrary
#endif
                     <*> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary <@> []
                     <*> arbitrary <@> []        <@> []
                     <@> []        <@> []        <@> []
                     <@> []        <@> []        <@> []
                     <@> []        <@> []        <@> []
                     <@> []        <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary
#if MIN_VERSION_ghc_boot(8,1,0)
                     <*> arbitrary
#endif

# if MIN_VERSION_ghc_boot(8,1,0)
deriving instance Generic (DbModule instunitid compid unitid modulename mod)
instance (Arbitrary instunitid, Arbitrary compid, Arbitrary unitid,
          Arbitrary modulename, Arbitrary mod)
  => Arbitrary (DbModule instunitid compid unitid modulename mod) where
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
