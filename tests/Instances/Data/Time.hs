{-# LANGUAGE CPP #-}

#if MIN_VERSION_time(1,8,0)
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.Data.Time
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for 'SystemTime' values.
-}
module Instances.Data.Time () where

#if MIN_VERSION_time(1,8,0)
import Data.Time.Clock.System (SystemTime(..))
import GHC.Generics (Generic)
import Instances.Utils.GenericArbitrary (genericArbitrary)
import Test.QuickCheck (Arbitrary(..))

instance Arbitrary SystemTime where
    arbitrary = genericArbitrary

deriving instance Generic SystemTime
#endif
