{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Binary
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for 'Decoder's.
-}
module Instances.Data.Binary () where

import Data.Binary.Get.Internal (Decoder(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), oneof)
import Test.QuickCheck.Instances ()

instance Arbitrary a => Arbitrary (Decoder a) where
    arbitrary = oneof [ Fail      <$> arbitrary <*> arbitrary
                      , Partial   <$> arbitrary
                      , Done      <$> arbitrary <*> arbitrary
                      , BytesRead <$> arbitrary <*> arbitrary
                      ]
