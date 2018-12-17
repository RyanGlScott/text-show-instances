{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.ShortText
Copyright:   (C) 2014-2018 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for @ShortText@ type.

/Since: next/
-}
module TextShow.Data.ShortText where

import           Data.Text.Short    (ShortText, toString)

import           TextShow           (TextShow (showb))
import           TextShow.Data.Char (showbString)


-- | /Since: next/
instance TextShow ShortText where
    showb = showbString . toString
    {-# INLINE showb #-}
