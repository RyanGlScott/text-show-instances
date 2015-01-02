{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Tagged
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides an 'Arbitrary' instance for 'Tagged' values.
-}
module Instances.Data.Tagged () where

import Data.Tagged (Tagged(..))
import Test.Tasty.QuickCheck (Arbitrary)

deriving instance Arbitrary b => Arbitrary (Tagged s b)