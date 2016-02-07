{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Compiler.Hoopl
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @hoopl@ library.
-}
module Instances.Compiler.Hoopl () where

import Compiler.Hoopl (Label, LabelMap, LabelSet, Pointed(..),
                       UniqueMap, UniqueSet, C, mapFromList, setFromList)
#if MIN_VERSION_hoopl(3,9,0)
import Compiler.Hoopl.Internals (uniqueToLbl)
#else
import Compiler.Hoopl (Unique, intToUnique)
import Compiler.Hoopl.GHC (uniqueToLbl)
#endif
import Compiler.Hoopl.Passes.Dominator (DominatorNode(..), DominatorTree(..), DPath(..))

import Instances.Utils ((<@>))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary Label where
    arbitrary = uniqueToLbl <$> arbitrary

instance Arbitrary v => Arbitrary (LabelMap v) where
    arbitrary = mapFromList <$> arbitrary

instance Arbitrary LabelSet where
    arbitrary = setFromList <$> arbitrary

instance Arbitrary a => Arbitrary (Pointed C C a) where
    arbitrary = oneof [pure Bot, PElem <$> arbitrary, pure Top]

#if !(MIN_VERSION_hoopl(3,9,0))
instance Arbitrary Unique where
    arbitrary = intToUnique <$> arbitrary
#endif

instance Arbitrary v => Arbitrary (UniqueMap v) where
    arbitrary = mapFromList <$> arbitrary

instance Arbitrary UniqueSet where
    arbitrary = setFromList <$> arbitrary

instance Arbitrary DominatorNode where
    arbitrary = oneof [pure Entry, Labelled <$> arbitrary]

instance Arbitrary DominatorTree where
    arbitrary = Dominates <$> arbitrary <@> [fDominatorTree]
--     arbitrary = Dominates <$> arbitrary <*> arbitrary

deriving instance Arbitrary DPath

-------------------------------------------------------------------------------
-- Workarounds to make Arbitrary instances faster
-------------------------------------------------------------------------------

fDominatorTree :: DominatorTree
fDominatorTree = Dominates Entry []
