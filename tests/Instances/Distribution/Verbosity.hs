{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Verbosity
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides an 'Arbitrary' instance for 'Verbosity' values.
-}
module Instances.Distribution.Verbosity () where

import Distribution.Verbosity (Verbosity)
import Test.Tasty.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance Arbitrary Verbosity where
    arbitrary = arbitraryBoundedEnum