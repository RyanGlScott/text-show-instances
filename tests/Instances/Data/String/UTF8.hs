{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.String.UTF8
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides an 'Arbitrary' instance for 'UTF8' strings.
-}
module Instances.Data.String.UTF8 () where

#if !(MIN_VERSION_base(4,8,0))
import Data.Functor ((<$>))
#endif
import Data.String.UTF8 (UTF8, fromRep)

import Test.Tasty.QuickCheck (Arbitrary(..))

instance Arbitrary a => Arbitrary (UTF8 a) where
    arbitrary = fromRep <$> arbitrary
