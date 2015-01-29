{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Utils
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for 'NubList's and 'NubListR's.
-}
module Instances.Distribution.Utils () where

import Data.Functor ((<$>))
import Distribution.Utils.NubList (NubList, toNubList, NubListR, toNubListR)
import Test.Tasty.QuickCheck (Arbitrary(..))

instance (Arbitrary a, Ord a) => Arbitrary (NubList a) where
    arbitrary = toNubList <$> arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (NubListR a) where
    arbitrary = toNubListR <$> arbitrary