{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Vector
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for 'Vector' types.
-}
module Instances.Data.Vector () where

import qualified Data.Vector as B (Vector)
import           Data.Vector.Fusion.Stream.Size (Size(..))
import qualified Data.Vector.Generic as G (Vector)
import           Data.Vector.Generic (fromList)
import qualified Data.Vector.Primitive as P (Vector)
import           Data.Vector.Primitive (Prim)
import qualified Data.Vector.Storable as S (Vector)
import qualified Data.Vector.Unboxed as U (Vector)
import           Data.Vector.Unboxed (Unbox)

import           Foreign.Storable (Storable)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), Gen, oneof)

arbitraryVector :: (Arbitrary a, G.Vector v a) => Gen (v a)
arbitraryVector = fromList <$> arbitrary

instance Arbitrary a => Arbitrary (B.Vector a) where
    arbitrary = arbitraryVector

instance (Arbitrary a, Prim a) => Arbitrary (P.Vector a) where
    arbitrary = arbitraryVector

instance (Arbitrary a, Storable a) => Arbitrary (S.Vector a) where
    arbitrary = arbitraryVector

instance (Arbitrary a, Unbox a) => Arbitrary (U.Vector a) where
    arbitrary = arbitraryVector

instance Arbitrary Size where
    arbitrary = oneof [Exact <$> arbitrary, Max <$> arbitrary, pure Unknown]
