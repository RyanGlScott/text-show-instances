{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Trace.Hpc
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @hpc@ library.
-}
module Instances.Trace.Hpc () where

import Instances.Utils ((<@>))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)
import Test.QuickCheck.Instances ()

import Trace.Hpc.Mix (Mix(..), MixEntry, BoxLabel(..), CondBox(..))
import Trace.Hpc.Tix (Tix(..), TixModule(..))
import Trace.Hpc.Util (HpcPos, Hash, toHpcPos)

instance Arbitrary Mix where
    arbitrary = Mix <$> arbitrary <*> arbitrary <*> arbitrary
                    <*> arbitrary <@> [fMixEntry]
--     arbitrary = Mix <$> arbitrary <*> arbitrary <*> arbitrary
--                     <*> arbitrary <*> arbitrary

instance Arbitrary BoxLabel where
    arbitrary = oneof [ ExpBox      <$> arbitrary
                      , TopLevelBox <$> arbitrary
                      , LocalBox    <$> arbitrary
                      , BinBox      <$> arbitrary <*> arbitrary
                      ]

deriving instance Bounded CondBox
deriving instance Enum CondBox
instance Arbitrary CondBox where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Tix where
    arbitrary = Tix <$> arbitrary

instance Arbitrary TixModule where
    arbitrary = TixModule <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary HpcPos where
    arbitrary = toHpcPos <$> ((,,,) <$> arbitrary <*> arbitrary
                                    <*> arbitrary <*> arbitrary)

instance Arbitrary Hash where
    arbitrary = fromInteger <$> arbitrary

-------------------------------------------------------------------------------
-- Workarounds to make Arbitrary instances faster
-------------------------------------------------------------------------------

fMixEntry :: MixEntry
fMixEntry = (toHpcPos (0, 1, 2, 3), ExpBox True)
