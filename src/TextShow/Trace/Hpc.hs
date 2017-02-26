{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Trace.Hpc
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @hpc@ library.

/Since: 2/
-}
module TextShow.Trace.Hpc (
      showbMixPrec
    , showbBoxLabelPrec
    , showbCondBox
    , showbTixPrec
    , showbTixModulePrec
    , showbHpcPos
    , showbHash
    ) where

import Data.Monoid.Compat

import TextShow (TextShow(..), Builder, FromStringShow(..), singleton)
import TextShow.Data.Integral (showbIntPrec)
import TextShow.Data.Time ()
import TextShow.TH (deriveTextShow)

import Trace.Hpc.Mix (Mix, BoxLabel, CondBox)
import Trace.Hpc.Tix (Tix, TixModule)
import Trace.Hpc.Util (HpcPos, Hash, fromHpcPos)

-- | Convert a 'Mix' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbMixPrec :: Int -> Mix -> Builder
showbMixPrec = showbPrec
{-# INLINE showbMixPrec #-}

-- | Convert a 'BoxLabel' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbBoxLabelPrec :: Int -> BoxLabel -> Builder
showbBoxLabelPrec = showbPrec
{-# INLINE showbBoxLabelPrec #-}

-- | Convert a 'CondBox' to a 'Builder'.
--
-- /Since: 2/
showbCondBox :: CondBox -> Builder
showbCondBox = showb
{-# INLINE showbCondBox #-}

-- | Convert a 'Tix' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbTixPrec :: Int -> Tix -> Builder
showbTixPrec = showbPrec
{-# INLINE showbTixPrec #-}

-- | Convert a 'TixModule' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbTixModulePrec :: Int -> TixModule -> Builder
showbTixModulePrec = showbPrec
{-# INLINE showbTixModulePrec #-}

-- | Convert a 'HpcPos' to a 'Builder'.
--
-- /Since: 2/
showbHpcPos :: HpcPos -> Builder
showbHpcPos hp = case fromHpcPos hp of
    (l1, c1, l2, c2) -> showbIntPrec 0 l1
           <> (singleton ':' <> showbIntPrec 0 c1)
           <> (singleton '-' <> showbIntPrec 0 l2)
           <> (singleton ':' <> showbIntPrec 0 c2)
{-# INLINE showbHpcPos #-}

-- | Convert a 'Hash' to a 'Builder'.
--
-- /Since: 2/
showbHash :: Hash -> Builder
showbHash = showb . FromStringShow
{-# INLINE showbHash #-}

$(deriveTextShow ''Mix)
$(deriveTextShow ''BoxLabel)
$(deriveTextShow ''CondBox)
$(deriveTextShow ''Tix)
$(deriveTextShow ''TixModule)

instance TextShow HpcPos where
    showb = showbHpcPos
    {-# INLINE showb #-}

instance TextShow Hash where
    showb = showbHash
    {-# INLINE showb #-}
