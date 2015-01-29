{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Simple.Program.Db
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Simple.Program.Db@
module of the @Cabal@ library.
-}
module Instances.Distribution.Simple.Program.Db () where

import Data.Functor ((<$>))

import Distribution.Simple.Program.Db (ProgramDb, emptyProgramDb, restoreProgramDb)

import Instances.Distribution.Simple.Program.Types ()
import Instances.Utils ((<@>))

import Test.Tasty.QuickCheck (Arbitrary(..))

instance Arbitrary ProgramDb where
    arbitrary = restoreProgramDb <$> arbitrary <@> emptyProgramDb