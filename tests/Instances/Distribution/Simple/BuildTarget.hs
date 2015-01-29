{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Simple.BuildTarget
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Simple.BuildTarget@
module of the @Cabal@ library.
-}
module Instances.Distribution.Simple.BuildTarget () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))

import Distribution.Simple.BuildTarget
    (BuildTarget(..), BuildTargetProblem(..), UserBuildTarget,
     UserBuildTargetProblem(..), readUserBuildTargets)

import Instances.Distribution.ModuleName            ()
import Instances.Distribution.Simple.LocalBuildInfo ()

import Test.Tasty.QuickCheck (Arbitrary(..), Gen, getNonEmpty, oneof, suchThat)

instance Arbitrary BuildTarget where
    arbitrary = oneof [ BuildTargetComponent <$> arbitrary
                      , BuildTargetModule    <$> arbitrary <*> arbitrary
                      , BuildTargetFile      <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary BuildTargetProblem where
    arbitrary = oneof [ BuildTargetExpected  <$> arbitrary <*> arbitrary <*> arbitrary
                      , BuildTargetNoSuch    <$> arbitrary <*> arbitrary
                      , BuildTargetAmbigious <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary UserBuildTarget where
    arbitrary = do
        ubt       <- noColonString
        ubtString <- oneof $ map pure [ ubt
                                      , ubt ++ ':':ubt
                                      , ubt ++ ':':ubt ++ ':':ubt
                                      ] 
        pure . head . snd $ readUserBuildTargets [ubtString]

instance Arbitrary UserBuildTargetProblem where
    arbitrary = UserBuildTargetUnrecognised <$> arbitrary

noColonString :: Gen String
noColonString = getNonEmpty <$> arbitrary `suchThat` (not . elem ':' . getNonEmpty)