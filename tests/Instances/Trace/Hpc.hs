{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Trace.Hpc
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @hpc@ library.
-}
module Instances.Trace.Hpc () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))

import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

import Trace.Hpc.Mix (Mix(..), BoxLabel(..), CondBox(..))
import Trace.Hpc.Tix (Tix(..), TixModule(..))
import Trace.Hpc.Util (HpcPos, Hash, toHpcPos)

instance Arbitrary Mix where
    arbitrary = Mix <$> arbitrary <*> arbitrary <*> arbitrary
                    <*> arbitrary <*> arbitrary

instance Arbitrary BoxLabel where
    arbitrary = oneof [ ExpBox      <$> arbitrary
                      , TopLevelBox <$> arbitrary
                      , LocalBox    <$> arbitrary
                      , BinBox      <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary CondBox where
    arbitrary = oneof $ map pure [GuardBinBox, CondBinBox, QualBinBox]

instance Arbitrary Tix where
    arbitrary = Tix <$> arbitrary

instance Arbitrary TixModule where
    arbitrary = TixModule <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary HpcPos where
    arbitrary = toHpcPos <$> ((,,,) <$> arbitrary <*> arbitrary
                                    <*> arbitrary <*> arbitrary)

instance Arbitrary Hash where
    arbitrary = fromInteger <$> arbitrary