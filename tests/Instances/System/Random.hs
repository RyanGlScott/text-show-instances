{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.System.Random
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides an 'Arbitrary' instance for 'StdGen' values.
-}
module Instances.System.Random () where

import Data.Functor ((<$>))
import System.Random (StdGen, mkStdGen)
import Test.Tasty.QuickCheck (Arbitrary(..))

instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary