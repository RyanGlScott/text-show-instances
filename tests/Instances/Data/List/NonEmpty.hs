{-# LANGUAGE CPP #-}
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

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>))
#endif

import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmpty(..))

import Test.Tasty.QuickCheck (Arbitrary(..))

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary