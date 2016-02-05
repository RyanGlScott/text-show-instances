{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Text.PrettyPrint
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @pretty@ library
(as well as 'Show' instances if using an old version of @pretty@).
-}
module Instances.Text.PrettyPrint () where

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

import           Text.PrettyPrint.HughesPJ (Doc, Mode(..), Style(..),
                                            TextDetails(..), text)
#if MIN_VERSION_pretty(1,1,2)
import           Text.PrettyPrint.HughesPJClass (PrettyLevel(..))
#endif
#if MIN_VERSION_pretty(1,1,3)
import qualified Text.PrettyPrint.Annotated.HughesPJ as Annot (Doc, text)
import           Text.PrettyPrint.Annotated.HughesPJ (AnnotDetails(..), Span(..))
import qualified Text.PrettyPrint.Annotated.HughesPJClass as Annot (PrettyLevel(..))
#endif

instance Arbitrary Doc where
    arbitrary = text <$> arbitrary

deriving instance Bounded Mode
deriving instance Enum Mode
instance Arbitrary Mode where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Style where
    arbitrary = Style <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TextDetails where
    arbitrary = oneof [Chr <$> arbitrary, Str <$> arbitrary, PStr <$> arbitrary]

#if MIN_VERSION_pretty(1,1,2)
deriving instance Arbitrary PrettyLevel
#else
deriving instance Show Mode
deriving instance Show Style
deriving instance Show TextDetails
#endif

#if MIN_VERSION_pretty(1,1,3)
instance Arbitrary a => Arbitrary (AnnotDetails a) where
    arbitrary = oneof [ pure AnnotStart
                      , NoAnnot <$> arbitrary <*> arbitrary
                      , AnnotEnd <$> arbitrary
                      ]

instance Arbitrary (Annot.Doc a) where
    arbitrary = Annot.text <$> arbitrary

deriving instance Arbitrary Annot.PrettyLevel

instance Arbitrary a => Arbitrary (Span a) where
    arbitrary = Span <$> arbitrary <*> arbitrary <*> arbitrary
#endif
