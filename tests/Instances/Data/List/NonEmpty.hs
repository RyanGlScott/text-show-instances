{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.List.NonEmpty
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides an 'Arbitrary' instance for 'NonEmpty' lists.
-}
module Instances.Data.List.NonEmpty () where

import Data.List.NonEmpty (NonEmpty(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
