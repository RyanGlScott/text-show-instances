{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Control.Applicative.Trans
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' functions for applicative functor transformers.

/Since: 0.1/
-}
module Text.Show.Text.Control.Applicative.Trans (
      showbBackwardsPrecWith
    , showbLiftPrecWith
    ) where

import Control.Applicative.Backwards (Backwards(..))
import Control.Applicative.Lift      (Lift(..))

import Prelude hiding (Show)

import Text.Show.Text (Show(showbPrec), Show1(..), Builder,
                       showbPrec1, showbUnaryWith)

#include "inline.h"

-- | Convert a 'Backwards' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 1/
showbBackwardsPrecWith :: Show1 f
                       => (Int -> a -> Builder)
                       -> Int -> Backwards f a -> Builder
showbBackwardsPrecWith sp p (Backwards x)
    = showbUnaryWith (showbPrecWith sp) "Backwards" p x
{-# INLINE showbBackwardsPrecWith #-}

-- | Convert a 'Lift' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbLiftPrecWith :: Show1 f
                  => (Int -> a -> Builder)
                  -> Int -> Lift f a -> Builder
showbLiftPrecWith sp p (Pure  x) = showbUnaryWith sp                 "Pure" p x
showbLiftPrecWith sp p (Other y) = showbUnaryWith (showbPrecWith sp) "Other" p y
{-# INLINE showbLiftPrecWith #-}

instance (Show1 f, Show a) => Show (Backwards f a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance Show1 f => Show1 (Backwards f) where
    showbPrecWith = showbBackwardsPrecWith
    INLINE_INST_FUN(showbPrecWith)

instance (Show1 f, Show a) => Show (Lift f a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance Show1 f => Show1 (Lift f) where
    showbPrecWith = showbLiftPrecWith
    INLINE_INST_FUN(showbPrecWith)
