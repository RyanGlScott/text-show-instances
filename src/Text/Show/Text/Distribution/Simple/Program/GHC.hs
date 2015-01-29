{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Simple.Program.GHC
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Simple.Program.GHC@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Simple.Program.GHC (
      showbGhcDynLinkMode
    , showbGhcMode
    , showbGhcOptimisationPrec
    , showbGhcOptionsPrec
    ) where

import Distribution.Simple.Program.GHC (GhcDynLinkMode, GhcMode,
                                        GhcOptimisation, GhcOptions)

import Text.Show.Text (Builder, showb, showbPrec)
import Text.Show.Text.Distribution.Simple.Setup ()
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'GhcDynLinkMode' to a 'Builder'.
-- 
-- /Since: 0.2/
showbGhcDynLinkMode :: GhcDynLinkMode -> Builder
showbGhcDynLinkMode = showb
{-# INLINE showbGhcDynLinkMode #-}

-- | Convert a 'GhcMode' to a 'Builder'.
-- 
-- /Since: 0.2/
showbGhcMode :: GhcMode -> Builder
showbGhcMode = showb
{-# INLINE showbGhcMode #-}

-- | Convert a 'GhcOptimisation' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbGhcOptimisationPrec :: Int -> GhcOptimisation -> Builder
showbGhcOptimisationPrec = showbPrec
{-# INLINE showbGhcOptimisationPrec #-}

-- | Convert a 'GhcOptions' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbGhcOptionsPrec :: Int -> GhcOptions -> Builder
showbGhcOptionsPrec = showbPrec
{-# INLINE showbGhcOptionsPrec #-}

$(deriveShow ''GhcDynLinkMode)
$(deriveShow ''GhcMode)
$(deriveShow ''GhcOptimisation)
$(deriveShow ''GhcOptions)