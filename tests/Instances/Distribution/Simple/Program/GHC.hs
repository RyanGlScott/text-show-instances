{-# LANGUAGE CPP, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Simple.Program.GHC
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Simple.Program.GHC@
module of the @Cabal@ library.
-}
module Instances.Distribution.Simple.Program.GHC () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif
import Data.Functor ((<$>))
import Distribution.Simple.Program.GHC (GhcDynLinkMode(..), GhcMode(..),
                                        GhcOptimisation(..), GhcOptions(..))
import Instances.Distribution.Simple.Setup ()
import Test.Tasty.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

deriving instance Bounded GhcDynLinkMode
deriving instance Enum GhcDynLinkMode
instance Arbitrary GhcDynLinkMode where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded GhcMode
deriving instance Enum GhcMode
instance Arbitrary GhcMode where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary GhcOptimisation where
    arbitrary = oneof [ pure GhcNoOptimisation
                      , pure GhcNormalOptimisation
                      , pure GhcMaximumOptimisation
                      , GhcSpecialOptimisation <$> arbitrary
                      ]

instance Arbitrary GhcOptions where
    arbitrary = GhcOptions <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary