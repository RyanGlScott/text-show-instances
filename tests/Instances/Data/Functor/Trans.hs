{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Functor.Trans
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for functor transformers.
-}
module Instances.Data.Functor.Trans () where

import Data.Functor.Reverse (Reverse(..))
import Test.QuickCheck (Arbitrary)

deriving instance Arbitrary (f a) => Arbitrary (Reverse f a)
