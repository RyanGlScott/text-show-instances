{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Simple.Test.Log
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Simple.Test.Log@
module of the @Cabal@ library.
-}
module Instances.Distribution.Simple.Test.Log () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>))
#endif

import Data.Functor ((<$>))

import Distribution.Simple.Test.Log (PackageLog(..), TestLogs(..), TestSuiteLog(..))

import Instances.Distribution.Compiler  ()
import Instances.Distribution.Package   ()
import Instances.Distribution.System    ()
import Instances.Distribution.TestSuite ()
import Instances.Utils ((<@>))

import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary PackageLog where
    arbitrary = PackageLog <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TestLogs where
    arbitrary = oneof [ TestLog   <$> arbitrary <*> arbitrary <*> arbitrary
                      , GroupLogs <$> arbitrary <@> [fTestLogs]
                      ]
--     arbitrary = oneof [ TestLog   <$> arbitrary <*> arbitrary <*> arbitrary
--                       , GroupLogs <$> arbitrary <*> arbitrary
--                       ]

fTestLogs :: TestLogs
fTestLogs = GroupLogs "" []

instance Arbitrary TestSuiteLog where
    arbitrary = TestSuiteLog <$> arbitrary <*> arbitrary <*> arbitrary