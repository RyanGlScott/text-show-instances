{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Simple.PackageIndex
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Simple.PackageIndex@
module of the @Cabal@ library.
-}
module Instances.Distribution.Simple.PackageIndex () where

import Data.Functor ((<$>))

import Distribution.Package (PackageInstalled)
import Distribution.Simple.PackageIndex (PackageIndex, fromList)

import Test.Tasty.QuickCheck (Arbitrary(..))

instance (Arbitrary a, PackageInstalled a) => Arbitrary (PackageIndex a) where
    arbitrary = fromList <$> arbitrary