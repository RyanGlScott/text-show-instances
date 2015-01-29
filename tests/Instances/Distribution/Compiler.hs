{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Compiler
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Compiler@
module of the @Cabal@ library.
-}
module Instances.Distribution.Compiler (fCompilerId) where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))
import Data.Version (Version(..))

import Distribution.Compiler (AbiTag(..), CompilerFlavor(..),
                              CompilerId(..), CompilerInfo(..))

import Instances.Language.Haskell.Extension ()
import Instances.Miscellaneous              ()
import Instances.Utils ((<@>))

import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary AbiTag where
    arbitrary = oneof [pure NoAbiTag, AbiTag <$> arbitrary]

instance Arbitrary CompilerFlavor where
    arbitrary = oneof [ pure GHC
                      , pure GHCJS
                      , pure NHC
                      , pure YHC
                      , pure Hugs
                      , pure HBC
                      , pure Helium
                      , pure JHC
                      , pure LHC
                      , pure UHC
                      , HaskellSuite  <$> arbitrary
                      , OtherCompiler <$> arbitrary
                      ]

instance Arbitrary CompilerId where
    arbitrary = CompilerId <$> arbitrary <*> arbitrary

instance Arbitrary CompilerInfo where
    arbitrary = CompilerInfo fCompilerId <$> arbitrary <@> (Just [fCompilerId])
                                         <*> arbitrary <*> arbitrary

-------------------------------------------------------------------------------
-- Workarounds to make Arbitrary instances faster
-------------------------------------------------------------------------------

fCompilerId :: CompilerId
fCompilerId = CompilerId GHC $ Version [] []