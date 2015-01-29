{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Simple.Setup
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Simple.Setup@
module of the @Cabal@ library.
-}
module Instances.Distribution.Simple.Setup () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))

import Distribution.Simple.Setup
    (BuildFlags(..), CleanFlags(..), ConfigFlags(..), CopyFlags(..), Flag(..),
     HaddockFlags(..), HscolourFlags(..), InstallFlags(..), RegisterFlags(..),
     ReplFlags(..), SDistFlags(..), TestShowDetails)

import Instances.Distribution.Compiler           ()
import Instances.Distribution.Package            ()
import Instances.Distribution.PackageDescription ()
import Instances.Distribution.Simple.Compiler    ()
import Instances.Distribution.Simple.InstallDirs ()
import Instances.Distribution.Simple.Program.Db  ()
import Instances.Distribution.Utils              ()
import Instances.Distribution.Verbosity          ()

import Test.Tasty.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

instance Arbitrary BuildFlags where
    arbitrary = BuildFlags <$> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CleanFlags where
    arbitrary = CleanFlags <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ConfigFlags where
    arbitrary = ConfigFlags <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CopyFlags where
    arbitrary = CopyFlags <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Flag a) where
    arbitrary = oneof [Flag <$> arbitrary, pure NoFlag]

instance Arbitrary HaddockFlags where
    arbitrary = HaddockFlags <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary HscolourFlags where
    arbitrary = HscolourFlags <$> arbitrary <*> arbitrary <*> arbitrary
                              <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary InstallFlags where
    arbitrary = InstallFlags <$> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary

instance Arbitrary RegisterFlags where
    arbitrary = RegisterFlags <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                              <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ReplFlags where
    arbitrary = ReplFlags <$> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary

instance Arbitrary SDistFlags where
    arbitrary = SDistFlags <$> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary

instance Arbitrary TestShowDetails where
    arbitrary = arbitraryBoundedEnum