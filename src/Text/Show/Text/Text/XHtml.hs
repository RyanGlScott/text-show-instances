{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Text.XHtml
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @xhtml@ library.

/Since: 0.1/
-}
module Text.Show.Text.Text.XHtml (
      showbHtml
    , showbHtmlList
    , showbHtmlAttr
    , showbHotLinkPrec
    , showbHtmlTable
    ) where

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (mconcat)
#endif

import Prelude hiding (Show)

import Text.Show.Text (Show(..), Builder, FromStringShow(..), fromString)
import Text.Show.Text.Data.Char (showbString)
import Text.Show.Text.TH (deriveShowPragmas, defaultInlineShowbPrec)
import Text.Show.Text.Utils ((<>), s)
import Text.XHtml.Frameset (Html, HtmlAttr, HotLink,
                            htmlAttrPair, renderHtmlFragment)
import Text.XHtml.Table (HtmlTable)

#include "inline.h"

-- | Convert an 'Html' value to a 'Builder'.
-- 
-- /Since: 0.1/
showbHtml :: Html -> Builder
showbHtml = fromString . renderHtmlFragment
{-# INLINE showbHtml #-}

-- | Convert a list of 'Html' values to a 'Builder'.
-- 
-- /Since: 0.1/
showbHtmlList :: [Html] -> Builder
showbHtmlList = mconcat . map showb
{-# INLINE showbHtmlList #-}

-- | Convert an 'HtmlAttr' to a 'Builder'.
-- 
-- /Since: 0.1/
showbHtmlAttr :: HtmlAttr -> Builder
showbHtmlAttr ha = case htmlAttrPair ha of
    (str, val) -> fromString str <> s '=' <> showbString val
{-# INLINE showbHtmlAttr #-}

-- | Convert a 'HotLink' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbHotLinkPrec :: Int -> HotLink -> Builder
showbHotLinkPrec = showbPrec
{-# INLINE showbHotLinkPrec #-}

-- | Convert an 'HtmlTable' to a 'Builder'.
-- 
-- /Since: 0.1/
showbHtmlTable :: HtmlTable -> Builder
showbHtmlTable = showb . FromStringShow
{-# INLINE showbHtmlTable #-}

instance Show Html where
    showb = showbHtml
    INLINE_INST_FUN(showb)
    
    showbList = showbHtmlList
    INLINE_INST_FUN(showbList)

instance Show HtmlAttr where
    showb = showbHtmlAttr
    INLINE_INST_FUN(showb)

$(deriveShowPragmas defaultInlineShowbPrec ''HotLink)

instance Show HtmlTable where
    showb = showbHtmlTable
    INLINE_INST_FUN(showb)