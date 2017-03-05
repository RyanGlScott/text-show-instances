{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.System.Process
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @process@ library
-}
module Instances.System.Process () where

#if MIN_VERSION_process(1,4,3)
import GHC.Generics (Generic)

import Instances.Miscellaneous ()
import Instances.Utils.GenericArbitrary (genericArbitrary)

import System.Process (CmdSpec(..), CreateProcess(..), StdStream(..))

import Test.QuickCheck (Arbitrary(..))

deriving instance Generic CmdSpec
instance Arbitrary CmdSpec where
    arbitrary = genericArbitrary

deriving instance Generic CreateProcess
instance Arbitrary CreateProcess where
    arbitrary = genericArbitrary

deriving instance Generic StdStream
instance Arbitrary StdStream where
    arbitrary = genericArbitrary
#endif
