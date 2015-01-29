{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Simple.Program.Types
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the
@Distribution.Simple.Program.Types@ module of the @Cabal@ library.
-}
module Instances.Distribution.Simple.Program.Types () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (Applicative((<*>), pure))
#endif

import Data.Functor ((<$>))

import Distribution.Simple.Program.Types
    (ConfiguredProgram(..), Program(..), ProgramLocation(..))

import Instances.Miscellaneous ()

import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary ConfiguredProgram where
    arbitrary = ConfiguredProgram <$> arbitrary <*> arbitrary <*> arbitrary
                    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Program where
    arbitrary = Program <$> arbitrary
                        <*> (simpleIO <$> arbitrary)
                        <*> (simpleIO <$> arbitrary)
                        <*> (simpleIO <$> arbitrary)

simpleIO :: Applicative f => a -> b -> c -> f a
simpleIO = const . const . pure

instance Arbitrary ProgramLocation where
    arbitrary = oneof $ map (<$> arbitrary) [UserSpecified, FoundOnSystem]