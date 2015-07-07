{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Control.Applicative.Trans
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for applicative functor transformers.

/Since: 2/
-}
module TextShow.Control.Applicative.Trans (
      showbBackwardsPrecWith
    , showbLiftPrecWith
    ) where

import Control.Applicative.Backwards (Backwards(..))
import Control.Applicative.Lift (Lift(..))

import TextShow (TextShow(showbPrec), TextShow1(..),
                 Builder, showbPrec1, showbUnaryWith)

#include "inline.h"

-- | Convert a 'Backwards' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 2/
showbBackwardsPrecWith :: TextShow1 f
                       => (Int -> a -> Builder)
                       -> Int -> Backwards f a -> Builder
showbBackwardsPrecWith sp p (Backwards x)
    = showbUnaryWith (showbPrecWith sp) "Backwards" p x
{-# INLINE showbBackwardsPrecWith #-}

-- | Convert a 'Lift' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbLiftPrecWith :: TextShow1 f
                  => (Int -> a -> Builder)
                  -> Int -> Lift f a -> Builder
showbLiftPrecWith sp p (Pure  x) = showbUnaryWith sp                 "Pure" p x
showbLiftPrecWith sp p (Other y) = showbUnaryWith (showbPrecWith sp) "Other" p y
{-# INLINE showbLiftPrecWith #-}

instance (TextShow1 f, TextShow a) => TextShow (Backwards f a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 f => TextShow1 (Backwards f) where
    showbPrecWith = showbBackwardsPrecWith
    INLINE_INST_FUN(showbPrecWith)

instance (TextShow1 f, TextShow a) => TextShow (Lift f a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 f => TextShow1 (Lift f) where
    showbPrecWith = showbLiftPrecWith
    INLINE_INST_FUN(showbPrecWith)
