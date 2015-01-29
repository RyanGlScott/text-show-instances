{-# LANGUAGE CPP, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Simple.InstallDirs
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Simple.InstallDirs@
module of the @Cabal@ library.
-}
module Instances.Distribution.Simple.InstallDirs () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif
import Data.Functor ((<$>))
import Distribution.Simple.InstallDirs
    (CopyDest(..), InstallDirs(..), PathTemplate, PathTemplateVariable(..),
     toPathTemplate)
import Test.Tasty.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

instance Arbitrary CopyDest where
    arbitrary = oneof [pure NoCopyDest, CopyTo <$> arbitrary]

instance Arbitrary a => Arbitrary (InstallDirs a) where
    arbitrary = InstallDirs <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary

instance Arbitrary PathTemplate where
    arbitrary = toPathTemplate <$> arbitrary

deriving instance Bounded PathTemplateVariable
deriving instance Enum PathTemplateVariable
instance Arbitrary PathTemplateVariable where
    arbitrary = arbitraryBoundedEnum