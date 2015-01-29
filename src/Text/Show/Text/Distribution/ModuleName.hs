{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.ModuleName
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'ModuleName's.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.ModuleName (showbModuleNamePrec) where

import Distribution.ModuleName (ModuleName)

import Text.Show.Text (Builder, showbPrec)
import Text.Show.Text.TH (deriveShowPragmas, defaultInlineShowbPrec)

-- | Convert a 'ModuleName' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbModuleNamePrec :: Int -> ModuleName -> Builder
showbModuleNamePrec = showbPrec
{-# INLINE showbModuleNamePrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''ModuleName)