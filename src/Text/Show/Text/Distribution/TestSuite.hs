{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.TestSuite
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.TestSuite@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.TestSuite (
      showbOptionDescrPrec
    , showbOptionTypePrec
    , showbResultPrec
    ) where

import Distribution.TestSuite (OptionDescr, OptionType, Result)

import Text.Show.Text (Builder, showbPrec)
import Text.Show.Text.TH (deriveShow)

-- | Convert an 'OptionDescr' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbOptionDescrPrec :: Int -> OptionDescr -> Builder
showbOptionDescrPrec = showbPrec
{-# INLINE showbOptionDescrPrec #-}

-- | Convert an 'OptionType' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbOptionTypePrec :: Int -> OptionType -> Builder
showbOptionTypePrec = showbPrec
{-# INLINE showbOptionTypePrec #-}

-- | Convert a 'Result' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbResultPrec :: Int -> Result -> Builder
showbResultPrec = showbPrec
{-# INLINE showbResultPrec #-}

$(deriveShow ''OptionDescr)
$(deriveShow ''OptionType)
$(deriveShow ''Result)