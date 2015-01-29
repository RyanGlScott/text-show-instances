{-# LANGUAGE CPP #-}
{- {-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-} -}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Package
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Package@
module of the @Cabal@ library.
-}
module Instances.Distribution.Package () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif
import Data.Functor ((<$>))
import Distribution.Package (Dependency(..), InstalledPackageId(..),
                             PackageIdentifier(..), PackageKey(..), PackageName(..))
import Instances.Distribution.Version ()
import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary Dependency where
    arbitrary = Dependency <$> arbitrary <*> arbitrary

instance Arbitrary InstalledPackageId where
    arbitrary = pure $ InstalledPackageId ""
-- deriving instance Arbitrary InstalledPackageId

instance Arbitrary PackageIdentifier where
    arbitrary = PackageIdentifier <$> arbitrary <*> arbitrary

instance Arbitrary PackageKey where
    arbitrary = oneof [ PackageKey    <$> arbitrary <*> arbitrary <*> arbitrary
                      , OldPackageKey <$> arbitrary
                      ]

instance Arbitrary PackageName where
    arbitrary = pure $ PackageName ""
-- deriving instance Arbitrary PackageName