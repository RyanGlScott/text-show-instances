{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Semigroup
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for @Semigroup@ data types.
-}
module Instances.Data.Semigroup () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>))
#endif

import Data.Functor ((<$>))
import Data.Semigroup (Min(..), Max(..), First(..), Last(..),
                       WrappedMonoid(..), Option(..), Arg(..))

import Test.Tasty.QuickCheck (Arbitrary(..))

deriving instance Arbitrary a => Arbitrary (Min a)
deriving instance Arbitrary a => Arbitrary (Max a)
deriving instance Arbitrary a => Arbitrary (First a)
deriving instance Arbitrary a => Arbitrary (Last a)
deriving instance Arbitrary m => Arbitrary (WrappedMonoid m)
deriving instance Arbitrary a => Arbitrary (Option a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Arg a b) where
    arbitrary = Arg <$> arbitrary <*> arbitrary