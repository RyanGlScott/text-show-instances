{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Trace.Hpc
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @hpc@ library.

/Since: 0.1/
-}
module Text.Show.Text.Trace.Hpc (
      showbMixPrec
    , showbBoxLabelPrec
    , showbCondBox
    , showbTixPrec
    , showbTixModulePrec
    , showbHpcPos
    , showbHash
    ) where

import Prelude hiding (Show)

import Text.Show.Text (Show(showb, showbPrec), Builder, FromStringShow(..))
import Text.Show.Text.Data.Integral (showbIntPrec)
import Text.Show.Text.Data.Time ()
import Text.Show.Text.TH (deriveShow, deriveShowPragmas,
                          defaultInlineShowb, defaultInlineShowbPrec)
import Text.Show.Text.Utils ((<>), s)

import Trace.Hpc.Mix (Mix, BoxLabel, CondBox)
import Trace.Hpc.Tix (Tix, TixModule)
import Trace.Hpc.Util (HpcPos, Hash, fromHpcPos)

#include "inline.h"

-- | Convert a 'Mix' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbMixPrec :: Int -> Mix -> Builder
showbMixPrec = showbPrec
{-# INLINE showbMixPrec #-}

-- | Convert a 'BoxLabel' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbBoxLabelPrec :: Int -> BoxLabel -> Builder
showbBoxLabelPrec = showbPrec
{-# INLINE showbBoxLabelPrec #-}

-- | Convert a 'CondBox' to a 'Builder'.
-- 
-- /Since: 0.1/
showbCondBox :: CondBox -> Builder
showbCondBox = showb
{-# INLINE showbCondBox #-}

-- | Convert a 'Tix' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbTixPrec :: Int -> Tix -> Builder
showbTixPrec = showbPrec
{-# INLINE showbTixPrec #-}

-- | Convert a 'TixModule' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbTixModulePrec :: Int -> TixModule -> Builder
showbTixModulePrec = showbPrec
{-# INLINE showbTixModulePrec #-}

-- | Convert a 'HpcPos' to a 'Builder'.
-- 
-- /Since: 0.1/
showbHpcPos :: HpcPos -> Builder
showbHpcPos hp = case fromHpcPos hp of
    (l1, c1, l2, c2) -> showbIntPrec 0 l1
           <> (s ':' <> showbIntPrec 0 c1)
           <> (s '-' <> showbIntPrec 0 l2)
           <> (s ':' <> showbIntPrec 0 c2)
{-# INLINE showbHpcPos #-}

-- | Convert a 'Hash' to a 'Builder'.
-- 
-- /Since: 0.1/
showbHash :: Hash -> Builder
showbHash = showb . FromStringShow
{-# INLINE showbHash #-}

$(deriveShow                               ''Mix)
$(deriveShow                               ''BoxLabel)
$(deriveShowPragmas defaultInlineShowb     ''CondBox)
$(deriveShowPragmas defaultInlineShowbPrec ''Tix)
$(deriveShow                               ''TixModule)

instance Show HpcPos where
    showb = showbHpcPos
    INLINE_INST_FUN(showb)

instance Show Hash where
    showb = showbHash
    INLINE_INST_FUN(showb)