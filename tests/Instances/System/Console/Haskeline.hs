{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.System.Console.Haskeline
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @haskeline@ library.
-}
module Instances.System.Console.Haskeline () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))

import System.Console.Haskeline (Interrupt(..))
import System.Console.Haskeline.Completion (Completion(..))
import System.Console.Haskeline.History (History, addHistory, emptyHistory)

import Test.Tasty.QuickCheck (Arbitrary(..))

instance Arbitrary Interrupt where
    arbitrary = pure Interrupt

-- instance Arbitrary Prefs

instance Arbitrary Completion where
    arbitrary = Completion <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary History where
    arbitrary = flip addHistory emptyHistory <$> arbitrary