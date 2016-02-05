{-# LANGUAGE OverloadedStrings #-}

{-|
Module:      TextShow.Utils
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Miscellaneous utility functions.
-}
module TextShow.Utils (showbUnaryListWith) where

import TextShow (Builder, showbUnaryWith)

-- | This pattern is used frequently when showing container types.
showbUnaryListWith :: ([a] -> Builder) -> Int -> [a] -> Builder
showbUnaryListWith sl p = showbUnaryWith (const sl) "fromList" p
{-# INLINE showbUnaryListWith #-}
