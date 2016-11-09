{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric      #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Vector
Copyright:   (C) 2014-2016 Ryan Scott
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
import           Data.Vector.Generic (fromList)
import qualified Data.Vector.Primitive as P (Vector)
import           Data.Vector.Primitive (Prim)

#if __GLASGOW_HASKELL__ >= 704
import           GHC.Generics (Generic)
#else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
#endif

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Instances ()

instance (Arbitrary a, Prim a) => Arbitrary (P.Vector a) where
    arbitrary = fromList <$> arbitrary

instance Arbitrary Size where
    arbitrary = genericArbitrary

#if __GLASGOW_HASKELL__ >= 704
deriving instance Generic Size
#else
$(Generics.deriveAll0 ''Size)
#endif
