{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Simple.Hpc
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Simple.Hpc@
module of the @Cabal@ library.
-}
module Instances.Distribution.Simple.Hpc () where

import Distribution.Simple.Hpc (Way)
import Test.Tasty.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance Arbitrary Way where
    arbitrary = arbitraryBoundedEnum