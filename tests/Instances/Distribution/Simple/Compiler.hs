{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Simple.Compiler
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Simple.Compiler@
module of the @Cabal@ library.
-}
module Instances.Distribution.Simple.Compiler () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))

import Distribution.Simple.Compiler
    (Compiler(..), DebugInfoLevel, OptimisationLevel, PackageDB(..))

import Instances.Distribution.Compiler (fCompilerId)
import Instances.Utils ((<@>))

import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

instance Arbitrary Compiler where
    arbitrary = do
        string    <- arbitrary
        language  <- arbitrary
        extension <- arbitrary
        Compiler fCompilerId
             <$> arbitrary
             <@> [fCompilerId]
             <@> [(language,  string)]
             <@> [(extension, string)]
             <*> arbitrary
--     arbitrary = Compiler <$> arbitrary <*> arbitrary <*> arbitrary
--                          <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary DebugInfoLevel where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary OptimisationLevel where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary PackageDB where
    arbitrary = oneof [ pure GlobalPackageDB
                      , pure UserPackageDB
                      , SpecificPackageDB <$> arbitrary
                      ]