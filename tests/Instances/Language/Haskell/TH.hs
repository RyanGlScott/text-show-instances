{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Language.Haskell.TH
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @template-haskell@ library.
-}
module Instances.Language.Haskell.TH () where

import Instances.Utils.GenericArbitrary (genericArbitrary)

import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.PprLib (Doc, text)
import Language.Haskell.TH.Syntax
#if !(MIN_VERSION_template_haskell(2,8,0))
import Language.Haskell.TH.Syntax.Internals
#endif

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance Arbitrary Name where
    arbitrary = genericArbitrary

instance Arbitrary NameFlavour where
    arbitrary = genericArbitrary

deriving instance Bounded NameIs
deriving instance Enum NameIs
deriving instance Show NameIs
instance Arbitrary NameIs where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded NameSpace
deriving instance Enum NameSpace
instance Arbitrary NameSpace where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Doc where
    arbitrary = text <$> arbitrary

deriving instance Arbitrary ModName
deriving instance Arbitrary OccName
deriving instance Arbitrary PkgName
