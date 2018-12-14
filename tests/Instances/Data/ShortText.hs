{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.ShortText
Copyright:   (C) 2014-2018 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for 'ShortText' values.

-}

module Instances.Data.ShortText () where

import           Data.Text.Short (ShortText, fromString)

import           Test.QuickCheck (Arbitrary (..))

instance Arbitrary ShortText where
    arbitrary = fromString <$> arbitrary
