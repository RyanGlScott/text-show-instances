{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Control.Applicative.Trans
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for applicative functor transformers.
-}
module Instances.Control.Applicative.Trans () where

import Control.Applicative.Backwards (Backwards(..))
import Control.Applicative.Lift      (Lift(..))

import Data.Functor ((<$>))

import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

deriving instance Arbitrary (f a) => Arbitrary (Backwards f a)

instance (Arbitrary a, Arbitrary (f a)) => Arbitrary (Lift f a) where
    arbitrary = oneof [Pure <$> arbitrary, Other <$> arbitrary]