{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Control.Applicative.Trans
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for applicative functor transformers.

/Since: 2/
-}
module TextShow.Control.Applicative.Trans (
      liftShowbBackwardsPrec
    , liftShowbLiftPrec
    ) where

import Control.Applicative.Backwards (Backwards(..))
import Control.Applicative.Lift (Lift(..))

import TextShow (TextShow(..), TextShow1(..),
                 Builder, showbPrec1, showbUnaryWith)

#include "inline.h"

-- | Convert a 'Backwards' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 3/
liftShowbBackwardsPrec :: TextShow1 f
                       => (Int -> a -> Builder) -> ([a] -> Builder)
                       -> Int -> Backwards f a -> Builder
liftShowbBackwardsPrec sp sl p (Backwards x)
    = showbUnaryWith (liftShowbPrec sp sl) "Backwards" p x
{-# INLINE liftShowbBackwardsPrec #-}

-- | Convert a 'Lift' value to a 'Builder' with the given show functions and precedence.
--
-- /Since: 3/
liftShowbLiftPrec :: TextShow1 f
                  => (Int -> a -> Builder) -> ([a] -> Builder)
                  -> Int -> Lift f a -> Builder
liftShowbLiftPrec sp _  p (Pure  x) = showbUnaryWith sp                    "Pure" p x
liftShowbLiftPrec sp sl p (Other y) = showbUnaryWith (liftShowbPrec sp sl) "Other" p y
{-# INLINE liftShowbLiftPrec #-}

instance (TextShow1 f, TextShow a) => TextShow (Backwards f a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 f => TextShow1 (Backwards f) where
    liftShowbPrec = liftShowbBackwardsPrec
    INLINE_INST_FUN(liftShowbPrec)

instance (TextShow1 f, TextShow a) => TextShow (Lift f a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 f => TextShow1 (Lift f) where
    liftShowbPrec = liftShowbLiftPrec
    INLINE_INST_FUN(liftShowbPrec)
