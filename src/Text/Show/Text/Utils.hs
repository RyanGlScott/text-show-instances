{-|
Module:      Text.Show.Text.Utils
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Miscellaneous utility functions.
-}
module Text.Show.Text.Utils where

import Data.Monoid (Monoid(mappend))
import Text.Show.Text (Builder, singleton)

infixr 6 <>

-- | Infix 'mappend', defined here for backwards-compatibility with older versions
-- of @base@.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

-- | A shorter name for 'singleton' for convenience's sake (since it tends to be used
-- pretty often in @text-show-instances@).
s :: Char -> Builder
s = singleton
{-# INLINE s #-}