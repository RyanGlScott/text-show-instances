{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Simple.Hpc
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Simple.Hpc@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Simple.Hpc (showbWay) where

import Distribution.Simple.Hpc (Way)

import Text.Show.Text (Builder, showb)
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'Way' value to a 'Builder'.
-- 
-- /Since: 0.2/
showbWay :: Way -> Builder
showbWay = showb
{-# INLINE showbWay #-}

$(deriveShow ''Way)