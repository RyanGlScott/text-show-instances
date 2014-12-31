{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Semigroup
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for @Semigroup@ data types.
-}
module Instances.Data.Semigroup () where

import Data.Semigroup (Min(..), Max(..), First(..),
                       Last(..), WrappedMonoid(..), Option(..))
import Test.Tasty.QuickCheck (Arbitrary)

deriving instance Arbitrary a => Arbitrary (Min a)
deriving instance Arbitrary a => Arbitrary (Max a)
deriving instance Arbitrary a => Arbitrary (First a)
deriving instance Arbitrary a => Arbitrary (Last a)
deriving instance Arbitrary m => Arbitrary (WrappedMonoid m)
deriving instance Arbitrary a => Arbitrary (Option a)