{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      Instances.Data.Vector
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for 'Vector' types.
-}
module Instances.Data.VectorNonEmpty () where

import           Data.Vector.NonEmpty as B (NonEmptyVector, fromList)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), listOf1)
import           Test.QuickCheck.Instances ()
import           Data.Maybe (fromJust)

instance Arbitrary a => Arbitrary (B.NonEmptyVector a) where
    arbitrary = (fromJust . B.fromList) <$> (listOf1 arbitrary)
