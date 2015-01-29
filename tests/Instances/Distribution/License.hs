{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.License
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides an 'Arbitrary' instance for 'License's.
-}
module Instances.Distribution.License () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (pure)
#endif
import Data.Functor ((<$>))
import Distribution.License (License(..))
import Instances.Miscellaneous ()
import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary License where
    arbitrary = oneof [ GPL            <$> arbitrary
                      , AGPL           <$> arbitrary
                      , LGPL           <$> arbitrary
                      , pure BSD2
                      , pure BSD3
                      , pure BSD4
                      , pure MIT
                      , pure ISC
                      , MPL            <$> arbitrary
                      , Apache         <$> arbitrary
                      , pure PublicDomain
                      , pure AllRightsReserved
                      , pure UnspecifiedLicense
                      , pure OtherLicense
                      , UnknownLicense <$> arbitrary
                      ]