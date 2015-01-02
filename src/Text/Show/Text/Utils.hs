{-# LANGUAGE CPP, OverloadedStrings #-}
{-|
Module:      Text.Show.Text.Utils
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Miscellaneous utility functions.
-}
module Text.Show.Text.Utils where

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(mappend))
#endif
import Prelude hiding (Show)
import Text.Show.Text (Show, Builder, showbUnary, singleton)

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

-- | This pattern is used frequently when showing container types.
showbUnaryList :: Show a => Int -> [a] -> Builder
showbUnaryList p = showbUnary "fromList" p
{-# INLINE showbUnaryList #-}
