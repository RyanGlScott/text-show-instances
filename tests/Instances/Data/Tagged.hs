{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Tagged
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for 'Tagged' values.
-}
module Instances.Data.Tagged () where

import Data.Tagged (Tagged(..))
import Test.QuickCheck (Arbitrary)

deriving instance Arbitrary b => Arbitrary (Tagged s b)
