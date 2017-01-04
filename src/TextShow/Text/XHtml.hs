{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Text.XHtml
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @xhtml@ library.

/Since: 2/
-}
module TextShow.Text.XHtml (
      showbHtml
    , showbHtmlList
    , showbHtmlAttr
    , showbHotLinkPrec
    , showbHtmlTable
    ) where

import Data.Monoid.Compat

import Text.XHtml.Frameset (Html, HtmlAttr, HotLink,
                            htmlAttrPair, renderHtmlFragment)
import Text.XHtml.Table (HtmlTable)

import TextShow (TextShow(..), Builder, FromStringShow(..), fromString, singleton)
import TextShow.Data.Char (showbString)
import TextShow.TH (deriveTextShow)

#include "inline.h"

-- | Convert an 'Html' value to a 'Builder'.
--
-- /Since: 2/
showbHtml :: Html -> Builder
showbHtml = fromString . renderHtmlFragment
{-# INLINE showbHtml #-}

-- | Convert a list of 'Html' values to a 'Builder'.
--
-- /Since: 2/
showbHtmlList :: [Html] -> Builder
showbHtmlList = mconcat . map showb
{-# INLINE showbHtmlList #-}

-- | Convert an 'HtmlAttr' to a 'Builder'.
--
-- /Since: 2/
showbHtmlAttr :: HtmlAttr -> Builder
showbHtmlAttr ha = case htmlAttrPair ha of
    (str, val) -> fromString str <> singleton '=' <> showbString val
{-# INLINE showbHtmlAttr #-}

-- | Convert a 'HotLink' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbHotLinkPrec :: Int -> HotLink -> Builder
showbHotLinkPrec = showbPrec
{-# INLINE showbHotLinkPrec #-}

-- | Convert an 'HtmlTable' to a 'Builder'.
--
-- /Since: 2/
showbHtmlTable :: HtmlTable -> Builder
showbHtmlTable = showb . FromStringShow
{-# INLINE showbHtmlTable #-}

instance TextShow Html where
    showb = showbHtml
    INLINE_INST_FUN(showb)

    showbList = showbHtmlList
    INLINE_INST_FUN(showbList)

instance TextShow HtmlAttr where
    showb = showbHtmlAttr
    INLINE_INST_FUN(showb)

$(deriveTextShow ''HotLink)

instance TextShow HtmlTable where
    showb = showbHtmlTable
    INLINE_INST_FUN(showb)
