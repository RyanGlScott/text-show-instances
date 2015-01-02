{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Functor.Trans
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for functor transformers. Note that an instance for
the 'Identity' transformer is found in @text-show@, since it is a part of
@base-4.8.0.0@ and later.

/Since: 0.1/
-}
module Text.Show.Text.Data.Functor.Trans (
      showbComposePrec
    , showbConstantPrec
    , showbProductPrec
    , showbReversePrec
    , showbSumPrec
    ) where

import Data.Functor.Compose  (Compose(..))
import Data.Functor.Constant (Constant(..))
import Data.Functor.Product  (Product(..))
import Data.Functor.Reverse  (Reverse(..))
import Data.Functor.Sum      (Sum(..))

import Prelude hiding (Show)

import Text.Show.Text (Show(showbPrec), Show1(showbPrec1), Builder,
                       showbUnary, showbUnary1, showbBinary1)

#include "inline.h"

-- kludge to get type with the same instances as g a
newtype Apply g a = Apply (g a)

instance (Show1 g, Show a) => Show (Apply g a) where
    showbPrec p (Apply x) = showbPrec1 p x
    INLINE_INST_FUN(showbPrec)

-- | Convert a 'Compose' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbComposePrec :: (Functor f, Show1 f, Show1 g, Show a) => Int -> Compose f g a -> Builder
showbComposePrec p (Compose x) = showbUnary1 "Compose" p $ fmap Apply x
{-# INLINE showbComposePrec #-}

-- | Convert a 'Constant' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbConstantPrec :: Show a => Int -> Constant a b -> Builder
showbConstantPrec p (Constant x) = showbUnary "Constant" p x
{-# INLINE showbConstantPrec #-}

-- | Convert a 'Product' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbProductPrec :: (Show1 f, Show1 g, Show a) => Int -> Product f g a -> Builder
showbProductPrec p (Pair x y) = showbBinary1 "Pair" p x y
{-# INLINE showbProductPrec #-}

-- | Convert a 'Reverse' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbReversePrec :: (Show1 f, Show a) => Int -> Reverse f a -> Builder
showbReversePrec p (Reverse x) = showbUnary1 "Reverse" p x
{-# INLINE showbReversePrec #-}

-- | Convert a 'Sum' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbSumPrec :: (Show1 f, Show1 g, Show a) => Int -> Sum f g a -> Builder
showbSumPrec p (InL x) = showbUnary1 "InL" p x
showbSumPrec p (InR y) = showbUnary1 "InR" p y
{-# INLINE showbSumPrec #-}

instance (Functor f, Show1 f, Show1 g, Show a) => Show (Compose f g a) where
    showbPrec = showbComposePrec
    INLINE_INST_FUN(showbPrec)

instance (Functor f, Show1 f, Show1 g) => Show1 (Compose f g) where
    showbPrec1 = showbComposePrec
    INLINE_INST_FUN(showbPrec1)

instance Show a => Show (Constant a b) where
    showbPrec = showbConstantPrec
    INLINE_INST_FUN(showbPrec)

instance Show a => Show1 (Constant a) where
    showbPrec1 = showbConstantPrec
    INLINE_INST_FUN(showbPrec1)

instance (Show1 f, Show1 g, Show a) => Show (Product f g a) where
    showbPrec = showbProductPrec
    INLINE_INST_FUN(showbPrec)

instance (Show1 f, Show1 g) => Show1 (Product f g) where
    showbPrec1 = showbProductPrec
    INLINE_INST_FUN(showbPrec1)

instance (Show1 f, Show a) => Show (Reverse f a) where
    showbPrec = showbReversePrec
    INLINE_INST_FUN(showbPrec)

instance Show1 f => Show1 (Reverse f) where
    showbPrec1 = showbReversePrec
    INLINE_INST_FUN(showbPrec1)

instance (Show1 f, Show1 g, Show a) => Show (Sum f g a) where
    showbPrec = showbSumPrec
    INLINE_INST_FUN(showbPrec)

instance (Show1 f, Show1 g) => Show1 (Sum f g) where
    showbPrec1 = showbSumPrec
    INLINE_INST_FUN(showbPrec1)