{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Simple.CCompiler
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Simple.CCompiler@
module of the @Cabal@ library.
-}
module Instances.Distribution.Simple.CCompiler () where

import Distribution.Simple.CCompiler (CDialect(..))
import Test.Tasty.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

deriving instance Bounded CDialect
deriving instance Enum CDialect
instance Arbitrary CDialect where
    arbitrary = arbitraryBoundedEnum