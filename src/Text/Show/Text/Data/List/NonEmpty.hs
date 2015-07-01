{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.List.NonEmpty
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'NonEmpty' lists.

/Since: 0.1/
-}
module Text.Show.Text.Data.List.NonEmpty (showbNonEmptyPrecWith) where

import Data.List.NonEmpty (NonEmpty)

import Text.Show.Text (Show1(..), Builder)
import Text.Show.Text.TH (deriveShow, deriveShow1)

-- | Convert a 'NonEmpty' list to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 1/
showbNonEmptyPrecWith :: (Int -> a -> Builder) -> Int -> NonEmpty a -> Builder
showbNonEmptyPrecWith = showbPrecWith
{-# INLINE showbNonEmptyPrecWith #-}

$(deriveShow  ''NonEmpty)
$(deriveShow1 ''NonEmpty)
