{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.System.Random
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides an 'Arbitrary' instance for 'StdGen' values.
-}
module Instances.System.Random () where

#if !(MIN_VERSION_base(4,8,0))
import Data.Functor ((<$>))
#endif
import System.Random (StdGen, mkStdGen)
import Test.Tasty.QuickCheck (Arbitrary(..))

instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary
