{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Functor.Trans
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for functor transformers.
-}
module Instances.Data.Functor.Trans () where

import Data.Functor.Compose  (Compose(..))
#if !(MIN_VERSION_QuickCheck(2,9,0))
import Data.Functor.Constant (Constant(..))
#endif
import Data.Functor.Product  (Product(..))
import Data.Functor.Reverse  (Reverse(..))
import Data.Functor.Sum      (Sum(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), oneof)

deriving instance Arbitrary (f (g a)) => Arbitrary (Compose f g a)
#if !(MIN_VERSION_QuickCheck(2,9,0))
deriving instance Arbitrary a         => Arbitrary (Constant a b)
#endif
deriving instance Arbitrary (f a)     => Arbitrary (Reverse f a)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Product f g a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Sum f g a) where
    arbitrary = oneof [InL <$> arbitrary, InR <$> arbitrary]
