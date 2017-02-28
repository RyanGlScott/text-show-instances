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

'TextShow' instance for the 'Extension' data type.
Only provided if using @ghc-boot-th@.

/Since: 3.3/
-}
module TextShow.GHC.LanguageExtensions.Type () where

#if defined(MIN_VERSION_ghc_boot_th)
import GHC.LanguageExtensions.Type (Extension)
import TextShow.TH (deriveTextShow)

-- | /Since: 3.3/
$(deriveTextShow ''Extension)
#endif
