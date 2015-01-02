{-# LANGUAGE CPP #-}
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

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))
import Data.Sequence (ViewL(..), ViewR(..))

import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary a => Arbitrary (ViewL a) where
    arbitrary = oneof [pure EmptyL, (:<) <$> arbitrary <*> arbitrary]

instance Arbitrary a => Arbitrary (ViewR a) where
    arbitrary = oneof [pure EmptyR, (:>) <$> arbitrary <*> arbitrary]