{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      Instances.Data.Vector
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for the 'NonEmptyVector' type.
-}
module Instances.Data.VectorNonEmpty () where

import           Data.Maybe (fromJust)
import           Data.Vector.NonEmpty as B (NonEmptyVector, fromList)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), listOf1)
import           Test.QuickCheck.Instances ()

instance Arbitrary a => Arbitrary (B.NonEmptyVector a) where
    arbitrary = (fromJust . B.fromList) <$> (listOf1 arbitrary)
