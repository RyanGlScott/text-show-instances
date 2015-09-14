{-# LANGUAGE CPP               #-}
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
module TextShow.Utils (
      mtimesDefault
    , showbUnaryListWith
    , showbUnaryList
    ) where

#if MIN_VERSION_semigroups(0,17,0)
import Data.Semigroup (mtimesDefault)
#else
import Data.Semigroup (timesN)
#endif

import TextShow (TextShow(showbPrec), Builder, showbUnaryWith)
import TextShow.Data.List (showbListWith)

#if !(MIN_VERSION_semigroups(0,17,0))
-- | Repeat a value @n@ times.
--
-- > mtimesDefault n a = a <> a <> ... <> a  -- using <> (n-1) times
mtimesDefault :: (Integral b, Monoid a) => b -> a -> a
mtimesDefault = timesN . fromIntegral
{-# INLINE mtimesDefault #-}
#endif

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
