{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Containers
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types located in @containers@.
-}
module Instances.Data.Containers () where

import Data.Sequence (ViewL(..), ViewR(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), oneof)
import Test.QuickCheck.Instances ()

instance Arbitrary a => Arbitrary (ViewL a) where
    arbitrary = oneof [pure EmptyL, (:<) <$> arbitrary <*> arbitrary]

instance Arbitrary a => Arbitrary (ViewR a) where
    arbitrary = oneof [pure EmptyR, (:>) <$> arbitrary <*> arbitrary]
