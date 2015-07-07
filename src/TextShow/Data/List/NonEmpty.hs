{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.List.NonEmpty
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'NonEmpty' lists.

/Since: 2/
-}
module TextShow.Data.List.NonEmpty (showbNonEmptyPrecWith) where

import Data.List.NonEmpty (NonEmpty)

import TextShow (Builder, showbPrecWith)
import TextShow.TH (deriveTextShow, deriveTextShow1)

-- | Convert a 'NonEmpty' list to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 2/
showbNonEmptyPrecWith :: (Int -> a -> Builder) -> Int -> NonEmpty a -> Builder
showbNonEmptyPrecWith = showbPrecWith
{-# INLINE showbNonEmptyPrecWith #-}

$(deriveTextShow  ''NonEmpty)
$(deriveTextShow1 ''NonEmpty)
