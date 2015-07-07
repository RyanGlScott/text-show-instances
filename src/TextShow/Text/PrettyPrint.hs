{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Text.PrettyPrint
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @pretty@ library.

/Since: 2/
-}
module TextShow.Text.PrettyPrint (
      renderB
    , renderStyleB
    , showbMode
    , showbStylePrec
    , showbTextDetailsPrec
#if MIN_VERSION_pretty(1,1,2)
    , showbPrettyLevelPrec
#endif
    ) where

import Data.Monoid.Compat

import Text.PrettyPrint.HughesPJ (Doc, Mode, Style(..), TextDetails(..),
                                  fullRender, style)
#if MIN_VERSION_pretty(1,1,2)
import Text.PrettyPrint.HughesPJClass (PrettyLevel)
#endif

import TextShow (TextShow(showb, showbPrec), Builder, fromString, singleton)
import TextShow.TH (deriveTextShow)

#include "inline.h"

-- | Renders a 'Doc' to a 'Builder' using the default 'style'.
--
-- /Since: 2/
renderB :: Doc -> Builder
renderB = renderStyleB style
{-# INLINE renderB #-}

-- | Renders a 'Doc' to a 'Builder' using the given 'Style'.
--
-- /Since: 2/
renderStyleB :: Style -> Doc -> Builder
renderStyleB sty doc = fullRender (mode sty)
                                  (lineLength sty)
                                  (ribbonsPerLine sty)
                                  txtPrinter
                                  mempty
                                  doc
{-# INLINE renderStyleB #-}

txtPrinter :: TextDetails -> Builder -> Builder
txtPrinter (Chr c)   b = singleton c <> b
txtPrinter (Str s')  b = fromString s' <> b
txtPrinter (PStr s') b = fromString s' <> b
{-# INLINE txtPrinter #-}

-- | Convert a 'Mode' to a 'Builder'.
--
-- /Since: 2/
showbMode :: Mode -> Builder
showbMode = showb
{-# INLINE showbMode #-}

-- | Convert a 'Style' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbStylePrec :: Int -> Style -> Builder
showbStylePrec = showbPrec
{-# INLINE showbStylePrec #-}

-- | Convert 'TextDetails' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbTextDetailsPrec :: Int -> TextDetails -> Builder
showbTextDetailsPrec = showbPrec
{-# INLINE showbTextDetailsPrec #-}

#if MIN_VERSION_pretty(1,1,2)
-- | Convert a 'PrettyLevel' value to a 'Builder' with the given precedence.
-- This function is only available with @pretty-1.1.2.0@ or later.
--
-- /Since: 2/
showbPrettyLevelPrec :: Int -> PrettyLevel -> Builder
showbPrettyLevelPrec = showbPrec
{-# INLINE showbPrettyLevelPrec #-}
#endif

instance TextShow Doc where
    showb = renderB
    INLINE_INST_FUN(showb)

$(deriveTextShow ''Mode)
$(deriveTextShow ''Style)
$(deriveTextShow ''TextDetails)

#if MIN_VERSION_pretty(1,1,2)
$(deriveTextShow ''PrettyLevel)
#endif
