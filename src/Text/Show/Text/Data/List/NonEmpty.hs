{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.List.NonEmpty
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'NonEmpty' lists.

/Since: 0.1/
-}
module Text.Show.Text.Data.List.NonEmpty (showbNonEmptyPrec) where

import Data.List.NonEmpty (NonEmpty)

import Prelude hiding (Show)

import Text.Show.Text (Show(showbPrec), Builder)
import Text.Show.Text.TH (deriveShowPragmas, defaultInlineShowbPrec)

-- | Convert a 'NonEmpty' list to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbNonEmptyPrec :: Show a => Int -> NonEmpty a -> Builder
showbNonEmptyPrec = showbPrec
{-# INLINE showbNonEmptyPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''NonEmpty)