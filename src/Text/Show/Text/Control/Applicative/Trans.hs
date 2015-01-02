{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Control.Applicative.Trans
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for applicative functor transformers.

/Since: 0.1/
-}
module Text.Show.Text.Control.Applicative.Trans (
      showbBackwardsPrec
    , showbLiftPrec
    ) where

import Control.Applicative.Backwards (Backwards(..))
import Control.Applicative.Lift      (Lift(..))

import Prelude hiding (Show)

import Text.Show.Text (Show(showbPrec), Show1(showbPrec1), Builder,
                       showbUnary, showbUnary1)

#include "inline.h"

-- | Convert a 'Backwards' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbBackwardsPrec :: (Show1 f, Show a) => Int -> Backwards f a -> Builder
showbBackwardsPrec p (Backwards x) = showbUnary1 "Backwards" p x
{-# INLINE showbBackwardsPrec #-}

-- | Convert a 'Lift' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbLiftPrec :: (Show1 f, Show a) => Int -> Lift f a -> Builder
showbLiftPrec p (Pure  x) = showbUnary  "Pure"  p x
showbLiftPrec p (Other y) = showbUnary1 "Other" p y
{-# INLINE showbLiftPrec #-}

instance (Show1 f, Show a) => Show (Backwards f a) where
    showbPrec = showbBackwardsPrec
    INLINE_INST_FUN(showbPrec)

instance Show1 f => Show1 (Backwards f) where
    showbPrec1 = showbBackwardsPrec
    INLINE_INST_FUN(showbPrec1)

instance (Show1 f, Show a) => Show (Lift f a) where
    showbPrec = showbLiftPrec
    INLINE_INST_FUN(showbPrec)

instance Show1 f => Show1 (Lift f) where
    showbPrec1 = showbLiftPrec
    INLINE_INST_FUN(showbPrec1)