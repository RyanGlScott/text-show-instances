{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Compiler
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Compiler@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Compiler (
      showbAbiTagPrec
    , showbCompilerFlavorPrec
    , showbCompilerIdPrec
    , showbCompilerInfoPrec
    ) where

import Distribution.Compiler (AbiTag, CompilerFlavor, CompilerId, CompilerInfo)

import Text.Show.Text (Builder, showbPrec)
import Text.Show.Text.Language.Haskell.Extension ()
import Text.Show.Text.TH (deriveShow, deriveShowPragmas, defaultInlineShowbPrec)

-- | Convert an 'AbiTag' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbAbiTagPrec :: Int -> AbiTag -> Builder
showbAbiTagPrec = showbPrec
{-# INLINE showbAbiTagPrec #-}

-- | Convert a 'CompilerFlavor' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbCompilerFlavorPrec :: Int -> CompilerFlavor -> Builder
showbCompilerFlavorPrec = showbPrec
{-# INLINE showbCompilerFlavorPrec #-}

-- | Convert a 'CompilerId' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbCompilerIdPrec :: Int -> CompilerId -> Builder
showbCompilerIdPrec = showbPrec
{-# INLINE showbCompilerIdPrec #-}

-- | Convert a 'CompilerInfo' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbCompilerInfoPrec :: Int -> CompilerInfo -> Builder
showbCompilerInfoPrec = showbPrec
{-# INLINE showbCompilerInfoPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''AbiTag)
$(deriveShow                               ''CompilerFlavor)
$(deriveShowPragmas defaultInlineShowbPrec ''CompilerId)
$(deriveShow                               ''CompilerInfo)