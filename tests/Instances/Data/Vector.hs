{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Vector
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for 'Vector' types.
-}
module Instances.Data.Vector () where

import           Data.Vector.Fusion.Bundle.Size (Size(..))
import           Data.Vector.Generic (fromList)
import qualified Data.Vector.Primitive as P (Vector)
import           Data.Vector.Primitive (Prim)

import           GHC.Generics (Generic)

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Instances ()

instance (Arbitrary a, Prim a) => Arbitrary (P.Vector a) where
    arbitrary = fromList <$> arbitrary

instance Arbitrary Size where
    arbitrary = genericArbitrary

deriving instance Generic Size
