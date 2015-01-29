{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.InstalledPackageInfo
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.InstalledPackageInfo@
module of the @Cabal@ library.
-}
module Instances.Distribution.InstalledPackageInfo () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>))
#endif

import Data.Functor ((<$>))

import Distribution.InstalledPackageInfo
    (ExposedModule(..), InstalledPackageInfo_(..), OriginalModule(..))

import Instances.Distribution.License    ()
import Instances.Distribution.ModuleName (fModuleName)
import Instances.Distribution.Package    ()
import Instances.Utils ((<@>))

import Test.Tasty.QuickCheck (Arbitrary(..))

instance Arbitrary ExposedModule where
    arbitrary = ExposedModule fModuleName <$> arbitrary <*> arbitrary

instance Arbitrary m => Arbitrary (InstalledPackageInfo_ m) where
    arbitrary = do
        string <- arbitrary
        InstalledPackageInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
               <@> string    <@> string    <@> string    <@> string    <@> string
               <@> string    <@> string    <@> string    <@> string    <*> arbitrary
               <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <@> [string]
               <@> [string]  <@> string    <@> [string]  <@> [string]  <@> [string]
               <@> [string]  <@> [string]  <*> arbitrary <@> [string]  <@> [string]
               <@> [string]  <@> [string]  <@> [string]  <@> [string]  <*> arbitrary
--     arbitrary = InstalledPackageInfo <$> arbitrary <*> arbitrary <*> arbitrary
--                     <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                     <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                     <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                     <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                     <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                     <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                     <*> arbitrary

instance Arbitrary OriginalModule where
    arbitrary = OriginalModule <$> arbitrary <@> fModuleName