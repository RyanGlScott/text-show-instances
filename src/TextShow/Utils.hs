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
module TextShow.Utils (showbUnaryListWith, showbUnaryList) where

import TextShow (TextShow(showbPrec), Builder, showbUnaryWith)
import TextShow.Data.List (showbListWith)

-- | This pattern is used frequently when showing container types.
showbUnaryListWith :: (a -> Builder) -> Int -> [a] -> Builder
showbUnaryListWith sp p = showbUnaryWith (const $ showbListWith sp) "fromList" p
{-# INLINE showbUnaryListWith #-}

-- | This pattern is used frequently when showing container types.
--
-- We define this separately from 'showbUnaryListWith' since calling 'showbPrec' on
-- a list may result in different output than 'showbListWith' (since a 'Show'
-- instance may override 'showbList').
showbUnaryList :: TextShow a => Int -> [a] -> Builder
showbUnaryList p = showbUnaryWith showbPrec "fromList" p
{-# INLINE showbUnaryList #-}
