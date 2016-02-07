{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.System.Random
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for 'StdGen' values.
-}
module Instances.System.Random () where

import Prelude ()
import Prelude.Compat

import System.Random (StdGen, mkStdGen)

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary
