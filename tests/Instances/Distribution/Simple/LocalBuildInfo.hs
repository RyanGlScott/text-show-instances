{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Simple.LocalBuildInfo
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the
@Distribution.Simple.LocalBuildInfo@ module of the @Cabal@ library.
-}
module Instances.Distribution.Simple.LocalBuildInfo () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))

import Distribution.Simple.LocalBuildInfo
    (Component(..), ComponentLocalBuildInfo(..), ComponentName(..),
     LibraryName(..), LocalBuildInfo(..))

import Instances.Distribution.InstalledPackageInfo ()
import Instances.Distribution.Package              ()
import Instances.Distribution.PackageDescription   ()
import Instances.Distribution.Simple.PackageIndex  ()
import Instances.Distribution.Simple.Program.Db    ()
import Instances.Distribution.Simple.Setup         ()

import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary Component where
    arbitrary = oneof [ CLib   <$> arbitrary
                      , CExe   <$> arbitrary
                      , CTest  <$> arbitrary
                      , CBench <$> arbitrary
                      ]

instance Arbitrary ComponentName where
    arbitrary = oneof [ pure CLibName
                      , CExeName   <$> arbitrary
                      , CTestName  <$> arbitrary
                      , CBenchName <$> arbitrary
                      ]

instance Arbitrary ComponentLocalBuildInfo where
    arbitrary = oneof
        [ LibComponentLocalBuildInfo   <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , ExeComponentLocalBuildInfo   <$> arbitrary <*> arbitrary
        , TestComponentLocalBuildInfo  <$> arbitrary <*> arbitrary
        , BenchComponentLocalBuildInfo <$> arbitrary <*> arbitrary
        ]

instance Arbitrary LibraryName where
    arbitrary = LibraryName <$> arbitrary

instance Arbitrary LocalBuildInfo where
    arbitrary = LocalBuildInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                               <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                               <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                               <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                               <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                               <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                               <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary