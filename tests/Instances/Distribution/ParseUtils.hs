{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.ParseUtils
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.ParseUtils@
module of the @Cabal@ library.
-}
module Instances.Distribution.ParseUtils () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>))
#endif
import Data.Functor ((<$>))
import Distribution.ParseUtils (Field(..), ParseResult(..), PError(..), PWarning(..))
import Instances.Utils ((<@>))
import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary Field where
    arbitrary = oneof [ F       <$> arbitrary <*> arbitrary <*> arbitrary
                      , Section <$> arbitrary <*> arbitrary <*> arbitrary <@> [fField]
                      , IfBlock <$> arbitrary <*> arbitrary <@> [fField]  <@> [fField]
                      ]
--     arbitrary = oneof [ F       <$> arbitrary <*> arbitrary <*> arbitrary
--                       , Section <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                       , IfBlock <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                       ]

instance Arbitrary a => Arbitrary (ParseResult a) where
    arbitrary = oneof [ParseFailed <$> arbitrary, ParseOk <$> arbitrary <*> arbitrary]

instance Arbitrary PError where
    arbitrary = oneof [ AmbiguousParse <$> arbitrary <*> arbitrary
                      , NoParse        <$> arbitrary <*> arbitrary
                      , TabsError      <$> arbitrary
                      , FromString     <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary PWarning where
    arbitrary = oneof [PWarning <$> arbitrary, UTFWarning <$> arbitrary <*> arbitrary]

-------------------------------------------------------------------------------
-- Workarounds to make Arbitrary instances faster
-------------------------------------------------------------------------------

fField :: Field
fField = F 0 "" ""