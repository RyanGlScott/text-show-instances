{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Simple.Configure
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Simple.Configure@
module of the @Cabal@ library.
-}
module Instances.Distribution.Simple.Configure () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))

import Distribution.Simple.Configure (ConfigStateFileError(..))

import Instances.Distribution.Package ()
import Instances.Utils ((<@>))

import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary ConfigStateFileError where
    arbitrary = oneof
        [ pure ConfigStateFileNoHeader
        , pure ConfigStateFileBadHeader
        , pure ConfigStateFileNoParse
        , pure ConfigStateFileMissing
        , ConfigStateFileBadVersion <$> arbitrary <*> arbitrary <@> fECSFE
        ]
--     arbitrary = oneof
--         [ pure ConfigStateFileNoHeader
--         , pure ConfigStateFileBadHeader
--         , pure ConfigStateFileNoParse
--         , pure ConfigStateFileMissing
--         , ConfigStateFileBadVersion <$> arbitrary <*> arbitrary <*> arbitrary
--         ]

fECSFE :: Either ConfigStateFileError b
fECSFE = Left ConfigStateFileNoHeader