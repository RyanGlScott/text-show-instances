{-# LANGUAGE CPP             #-}

#if defined(MIN_VERSION_ghc_boot_th)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      TextShow.GHC.LanguageExtensions.Type
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for the 'Extension' data type.
This module only exports functions if using @ghc-boot-th@.

/Since: 3.3/
-}
module TextShow.GHC.LanguageExtensions.Type (
#if !defined(MIN_VERSION_ghc_boot_th)
    ) where
#else
      showbExtension
    ) where

import GHC.LanguageExtensions.Type (Extension)

import TextShow (TextShow(..), Builder)
import TextShow.TH (deriveTextShow)

-- | Convert an 'Extension' to a 'Builder'.
-- This function is only available when using @ghc-boot@.
--
-- /Since: 3.3/
showbExtension :: Extension -> Builder
showbExtension = showb

$(deriveTextShow ''Extension)
#endif
