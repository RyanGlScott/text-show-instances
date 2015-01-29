{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Simple.CCompiler
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Simple.CCompiler@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Simple.CCompiler (showbCDialect) where

import Distribution.Simple.CCompiler (CDialect)

import Text.Show.Text (Builder, showb)
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'CDialect' to a 'Builder'.
-- 
-- /Since: 0.2/
showbCDialect :: CDialect -> Builder
showbCDialect = showb
{-# INLINE showbCDialect #-}

$(deriveShow ''CDialect)