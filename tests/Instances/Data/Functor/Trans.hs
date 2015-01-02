{-# LANGUAGE CPP, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Functor.Trans
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for functor transformers.
-}
module Instances.Data.Functor.Trans () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>))
#endif

import Data.Functor ((<$>))
import Data.Functor.Compose  (Compose(..))
import Data.Functor.Constant (Constant(..))
import Data.Functor.Product  (Product(..))
import Data.Functor.Reverse  (Reverse(..))
import Data.Functor.Sum      (Sum(..))

import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

deriving instance Arbitrary (f (g a)) => Arbitrary (Compose f g a)
deriving instance Arbitrary a         => Arbitrary (Constant a b)
deriving instance Arbitrary (f a)     => Arbitrary (Reverse f a)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Product f g a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Sum f g a) where
    arbitrary = oneof [InL <$> arbitrary, InR <$> arbitrary]