{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Vector
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for 'Vector' types.
-}
module Instances.Data.Vector () where

#if MIN_VERSION_vector(0,11,0)
import           Data.Vector.Fusion.Bundle.Size (Size(..))
#else
import           Data.Vector.Fusion.Stream.Size (Size(..))
#endif
import qualified Data.Vector.Generic as G (Vector)
import           Data.Vector.Generic (fromList)
import qualified Data.Vector.Primitive as P (Vector)
import           Data.Vector.Primitive (Prim)
import           Data.Vector.Unboxed (Unbox)

import           Foreign.Storable (Storable)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), Gen, oneof)
import           Test.QuickCheck.Instances ()

arbitraryVector :: (Arbitrary a, G.Vector v a) => Gen (v a)
arbitraryVector = fromList <$> arbitrary

instance (Arbitrary a, Prim a) => Arbitrary (P.Vector a) where
    arbitrary = arbitraryVector

instance Arbitrary Size where
    arbitrary = oneof [Exact <$> arbitrary, Max <$> arbitrary, pure Unknown]
