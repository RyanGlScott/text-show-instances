{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Text.PrettyPrint
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @pretty@ library.

/Since: 0.1/
-}
module Text.Show.Text.Text.PrettyPrint (
      showbDoc
    , showbMode
    , showbStylePrec
    , showbTextDetailsPrec
#if MIN_VERSION_pretty(1,1,2)
    , showbPrettyLevelPrec
#endif
    ) where

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (mempty)
#endif

import Prelude hiding (Show)

import Text.PrettyPrint.HughesPJ (Doc, Mode, Style(..), TextDetails(..),
                                  fullRender, style)
#if MIN_VERSION_pretty(1,1,2)
import Text.PrettyPrint.HughesPJClass (PrettyLevel)
#endif
import Text.Show.Text (Show(showb, showbPrec), Builder, fromString)
import Text.Show.Text.TH (deriveShowPragmas, defaultInlineShowb, defaultInlineShowbPrec)
import Text.Show.Text.Utils ((<>), s)

#include "inline.h"

-- | Convert a 'Doc' to a 'Builder'.
-- 
-- /Since: 0.1/
showbDoc :: Doc -> Builder
showbDoc doc = fullRender (mode style) (lineLength style)
                          (ribbonsPerLine style)
                          txtPrinter mempty doc
{-# INLINE showbDoc #-}

txtPrinter :: TextDetails -> Builder -> Builder
txtPrinter (Chr c)   b = s c <> b
txtPrinter (Str s')  b = fromString s' <> b
txtPrinter (PStr s') b = fromString s' <> b
{-# INLINE txtPrinter #-}

-- | Convert a 'Mode' to a 'Builder'.
-- 
-- /Since: 0.1/
showbMode :: Mode -> Builder
showbMode = showb
{-# INLINE showbMode #-}

-- | Convert a 'Style' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbStylePrec :: Int -> Style -> Builder
showbStylePrec = showbPrec
{-# INLINE showbStylePrec #-}

-- | Convert 'TextDetails' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbTextDetailsPrec :: Int -> TextDetails -> Builder
showbTextDetailsPrec = showbPrec
{-# INLINE showbTextDetailsPrec #-}

#if MIN_VERSION_pretty(1,1,2)
-- | Convert a 'PrettyLevel' value to a 'Builder' with the given precedence.
-- This function is only available with @pretty-1.1.2.0@ or later.
-- 
-- /Since: 0.1/
showbPrettyLevelPrec :: Int -> PrettyLevel -> Builder
showbPrettyLevelPrec = showbPrec
{-# INLINE showbPrettyLevelPrec #-}
#endif

instance Show Doc where
    showb = showbDoc
    INLINE_INST_FUN(showb)

$(deriveShowPragmas defaultInlineShowb     ''Mode)
$(deriveShowPragmas defaultInlineShowbPrec ''Style)
$(deriveShowPragmas defaultInlineShowbPrec ''TextDetails)

#if MIN_VERSION_pretty(1,1,2)
$(deriveShowPragmas defaultInlineShowbPrec ''PrettyLevel)
#endif