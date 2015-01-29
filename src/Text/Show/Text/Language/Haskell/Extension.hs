{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Language.Haskell.Extension
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Language.Haskell.Extension@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Language.Haskell.Extension (
      showbExtensionPrec
    , showbKnownExtension
    , showbLanguagePrec
    ) where

import Language.Haskell.Extension (Extension, KnownExtension, Language)

import Text.Show.Text (Builder, showb, showbPrec)
import Text.Show.Text.TH (deriveShow)

-- | Convert an 'Extension' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbExtensionPrec :: Int -> Extension -> Builder
showbExtensionPrec = showbPrec
{-# INLINE showbExtensionPrec #-}

-- | Convert a 'KnownExtension' to a 'Builder'.
-- 
-- /Since: 0.2/
showbKnownExtension :: KnownExtension -> Builder
showbKnownExtension = showb
{-# INLINE showbKnownExtension #-}

-- | Convert a 'Language' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbLanguagePrec :: Int -> Language -> Builder
showbLanguagePrec = showbPrec
{-# INLINE showbLanguagePrec #-}

$(deriveShow ''Extension)
$(deriveShow ''KnownExtension)
$(deriveShow ''Language)