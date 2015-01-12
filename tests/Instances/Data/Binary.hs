{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Binary
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides an 'Arbitrary' instance for 'Decoder's.
-}
module Instances.Data.Binary () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>))
#endif

import Data.Binary.Get.Internal (Decoder(..))
import Data.Functor ((<$>))

import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary a => Arbitrary (Decoder a) where
    arbitrary = oneof [ Fail      <$> arbitrary <*> arbitrary
                      , Partial   <$> arbitrary
                      , Done      <$> arbitrary <*> arbitrary
                      , BytesRead <$> arbitrary <*> arbitrary
                      ]