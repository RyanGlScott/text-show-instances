{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.System.Locale
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for old 'TimeLocale' values.
-}
module Instances.System.Locale () where

import Prelude ()
import Prelude.Compat

import System.Locale (TimeLocale(..))

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary TimeLocale where
    arbitrary = TimeLocale <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
