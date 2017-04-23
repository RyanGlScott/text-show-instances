{-# LANGUAGE CPP             #-}

#if defined(MIN_VERSION_ghc_boot_th) && MIN_VERSION_ghc_boot_th(8,2,0)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      TextShow.GHC.ForeignSrcLang.Type
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for the 'ForeignSrcLang' data type.
Only provided if using @ghc-boot-th-8.2@ or later.

/Since: 3.3/
-}
module TextShow.GHC.ForeignSrcLang.Type () where

#if defined(MIN_VERSION_ghc_boot_th) && MIN_VERSION_ghc_boot_th(8,2,0)
import GHC.ForeignSrcLang.Type (ForeignSrcLang)
import TextShow.TH (deriveTextShow)

-- | /Since: next/
$(deriveTextShow ''ForeignSrcLang)
#endif
