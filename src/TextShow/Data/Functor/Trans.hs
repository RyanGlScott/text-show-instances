{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Functor.Trans
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for functor transformers. Note that an instance for
the 'Identity' transformer is found in @text-show@, since it is a part of
@base-4.8.0.0@ and later.

/Since: 2/
-}
module TextShow.Data.Functor.Trans (
      showbComposePrecWith
    , showbConstantPrecWith
    , showbProductPrecWith
    , showbReversePrecWith
    , showbSumPrecWith
    ) where

import Data.Functor.Compose  (Compose(..))
import Data.Functor.Constant (Constant(..))
import Data.Functor.Product  (Product(..))
import Data.Functor.Reverse  (Reverse(..))
import Data.Functor.Sum      (Sum(..))

import TextShow (TextShow(showbPrec), TextShow1(..), TextShow2(..),
                 Builder, showbPrec1, showbUnaryWith, showbBinaryWith)

#include "inline.h"

-- | Convert a 'Compose' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 2/
showbComposePrecWith :: (TextShow1 f, TextShow1 g)
                     => (Int -> a -> Builder)
                     -> Int -> Compose f g a -> Builder
showbComposePrecWith sp p (Compose x) =
    showbUnaryWith (showbPrecWith (showbPrecWith sp)) "Compose" p x
{-# INLINE showbComposePrecWith #-}

-- | Convert a 'Constant' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 2/
showbConstantPrecWith :: (Int -> a -> Builder) -> Int -> Constant a b -> Builder
showbConstantPrecWith sp p (Constant x) = showbUnaryWith sp "Constant" p x
{-# INLINE showbConstantPrecWith #-}

-- | Convert a 'Product' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 2/
showbProductPrecWith :: (TextShow1 f, TextShow1 g)
                     => (Int -> a -> Builder)
                     -> Int -> Product f g a -> Builder
showbProductPrecWith sp p (Pair x y) =
    showbBinaryWith (showbPrecWith sp) (showbPrecWith sp) "Pair" p x y
{-# INLINE showbProductPrecWith #-}

-- | Convert a 'Reverse' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 2/
showbReversePrecWith :: TextShow1 f
                     => (Int -> a -> Builder)
                     -> Int -> Reverse f a -> Builder
showbReversePrecWith sp p (Reverse x) = showbUnaryWith (showbPrecWith sp) "Reverse" p x
{-# INLINE showbReversePrecWith #-}

-- | Convert a 'Sum' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbSumPrecWith :: (TextShow1 f, TextShow1 g)
                 => (Int -> a -> Builder)
                 -> Int -> Sum f g a -> Builder
showbSumPrecWith sp p (InL x) = showbUnaryWith (showbPrecWith sp) "InL" p x
showbSumPrecWith sp p (InR y) = showbUnaryWith (showbPrecWith sp) "InR" p y
{-# INLINE showbSumPrecWith #-}

instance (TextShow1 f, TextShow1 g, TextShow a) => TextShow (Compose f g a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance (TextShow1 f, TextShow1 g) => TextShow1 (Compose f g) where
    showbPrecWith = showbComposePrecWith
    INLINE_INST_FUN(showbPrecWith)

instance TextShow a => TextShow (Constant a b) where
    showbPrec = showbConstantPrecWith showbPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow a => TextShow1 (Constant a) where
    showbPrecWith = showbPrecWith2 showbPrec
    INLINE_INST_FUN(showbPrecWith)

instance TextShow2 Constant where
    showbPrecWith2 sp _ = showbConstantPrecWith sp
    INLINE_INST_FUN(showbPrecWith2)

instance (TextShow1 f, TextShow1 g, TextShow a) => TextShow (Product f g a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance (TextShow1 f, TextShow1 g) => TextShow1 (Product f g) where
    showbPrecWith = showbProductPrecWith
    INLINE_INST_FUN(showbPrecWith)

instance (TextShow1 f, TextShow a) => TextShow (Reverse f a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 f => TextShow1 (Reverse f) where
    showbPrecWith = showbReversePrecWith
    INLINE_INST_FUN(showbPrecWith)

instance (TextShow1 f, TextShow1 g, TextShow a) => TextShow (Sum f g a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance (TextShow1 f, TextShow1 g) => TextShow1 (Sum f g) where
    showbPrecWith = showbSumPrecWith
    INLINE_INST_FUN(showbPrecWith)
