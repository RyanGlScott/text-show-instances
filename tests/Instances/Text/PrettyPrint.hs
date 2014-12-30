{-# LANGUAGE CPP, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Text.PrettyPrint
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @pretty@ library
(as well as 'Show' instances if using an old version of @pretty@).
-}
module Instances.Text.PrettyPrint () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))

import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

import Text.PrettyPrint.HughesPJ (Doc, Mode(..), Style(..), TextDetails(..), text)
#if MIN_VERSION_pretty(1,1,2)
import Text.PrettyPrint.HughesPJClass (PrettyLevel(..))
#endif

instance Arbitrary Doc where
    arbitrary = text <$> arbitrary

instance Arbitrary Mode where
    arbitrary = oneof $ map pure [PageMode, ZigZagMode, LeftMode, OneLineMode]

instance Arbitrary Style where
    arbitrary = Style <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TextDetails where
    arbitrary = oneof [Chr <$> arbitrary, Str <$> arbitrary, PStr <$> arbitrary]

#if MIN_VERSION_pretty(1,1,2)
deriving instance Arbitrary PrettyLevel
#else
deriving instance Show Mode
deriving instance Show Style
deriving instance Show TextDetails
#endif