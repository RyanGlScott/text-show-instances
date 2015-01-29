{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.TestSuite
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.TestSuite@
module of the @Cabal@ library.
-}
module Instances.Distribution.TestSuite () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif
import Data.Functor ((<$>))
import Distribution.TestSuite (OptionDescr(..), OptionType(..), Result(..))
import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary OptionDescr where
    arbitrary = OptionDescr <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary OptionType where
    arbitrary = oneof [ OptionFile   <$> arbitrary <*> arbitrary <*> arbitrary
                      , OptionString <$> arbitrary
                      , OptionNumber <$> arbitrary <*> arbitrary
                      , pure OptionBool
                      , OptionEnum   <$> arbitrary
                      , OptionSet    <$> arbitrary
                      , pure OptionRngSeed
                      ]

instance Arbitrary Result where
    arbitrary = oneof [pure Pass, Fail <$> arbitrary, Error <$> arbitrary]