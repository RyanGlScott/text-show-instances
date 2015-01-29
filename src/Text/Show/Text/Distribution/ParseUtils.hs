{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.ParseUtils
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.ParseUtils@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.ParseUtils (
      showbFieldPrec
    , showbParseResultPrec
    , showbPErrorPrec
    , showbPWarningPrec
    ) where

import Distribution.ParseUtils (Field, ParseResult, PError, PWarning)

import Prelude hiding (Show)

import Text.Show.Text (Show(showbPrec), Show1(showbPrec1), Builder)
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'Field' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbFieldPrec :: Int -> Field -> Builder
showbFieldPrec = showbPrec
{-# INLINE showbFieldPrec #-}

-- | Convert a 'ParseResult' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbParseResultPrec :: Show a => Int -> ParseResult a -> Builder
showbParseResultPrec = showbPrec
{-# INLINE showbParseResultPrec #-}

-- | Convert a 'PError' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbPErrorPrec :: Int -> PError -> Builder
showbPErrorPrec = showbPrec
{-# INLINE showbPErrorPrec #-}

-- | Convert a 'PWarning' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbPWarningPrec :: Int -> PWarning -> Builder
showbPWarningPrec = showbPrec
{-# INLINE showbPWarningPrec #-}

$(deriveShow ''Field)
$(deriveShow ''ParseResult)
$(deriveShow ''PError)
$(deriveShow ''PWarning)

instance Show1 ParseResult where
    showbPrec1 = showbPrec