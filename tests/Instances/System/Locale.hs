{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.System.Locale
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides an 'Arbitrary' instance for old 'TimeLocale' values.
-}
module Instances.System.Locale () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>))
#endif
import Data.Functor ((<$>))
import System.Locale (TimeLocale(..))
import Test.Tasty.QuickCheck (Arbitrary(..))

instance Arbitrary TimeLocale where
    arbitrary = TimeLocale <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary