{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Language.Haskell.Extension
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Language.Haskell.Extension@
module of the @Cabal@ library.
-}
module Instances.Language.Haskell.Extension () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (pure)
#endif
import Data.Functor ((<$>))
import Language.Haskell.Extension (Extension(..), KnownExtension, Language(..))
import Test.Tasty.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

instance Arbitrary Extension where
    arbitrary = oneof [ EnableExtension  <$> arbitrary
                      , DisableExtension <$> arbitrary
                      , UnknownExtension <$> arbitrary
                      ]

instance Arbitrary KnownExtension where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Language where
    arbitrary = oneof [pure Haskell98, pure Haskell2010, UnknownLanguage <$> arbitrary]