{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Text.PrettyPrint
Copyright:   (C) 2014-2016 Ryan Scott
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
#if MIN_VERSION_pretty(1,1,3)
    , renderAnnotB
    , renderStyleAnnotB
#endif
    , showbMode
    , showbStylePrec
    , showbTextDetailsPrec
#if MIN_VERSION_pretty(1,1,2)
    , showbPrettyLevelPrec
#endif
#if MIN_VERSION_pretty(1,1,3)
    , liftShowbAnnotDetailsPrec
    , showbPrettyLevelAnnotPrec
    , liftShowbSpanPrec
#endif
    ) where

import           Data.Monoid.Compat

import           Text.PrettyPrint.HughesPJ (Doc, Mode, Style(..), TextDetails(..),
                                            fullRender, style)
#if MIN_VERSION_pretty(1,1,2)
import           Text.PrettyPrint.HughesPJClass (PrettyLevel)
#endif
#if MIN_VERSION_pretty(1,1,3)
import qualified Text.PrettyPrint.Annotated.HughesPJ as Annot (Doc, fullRender, style)
import           Text.PrettyPrint.Annotated.HughesPJ (AnnotDetails, Span)
import qualified Text.PrettyPrint.Annotated.HughesPJClass as Annot (PrettyLevel)

import           TextShow (TextShow1(..))
import           TextShow.TH (deriveTextShow1)
#endif

import           TextShow (TextShow(..), Builder, fromString, singleton)
import           TextShow.TH (deriveTextShow)

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

#if MIN_VERSION_pretty(1,1,3)
-- | Renders an annotated 'Doc' to a 'Builder' using the default 'Annot.style'.
-- This function is only available with @pretty-1.1.3@ or later.
--
-- /Since: 3/
renderAnnotB :: Annot.Doc a -> Builder
renderAnnotB = renderStyleAnnotB Annot.style
{-# INLINE renderAnnotB #-}

-- | Renders an annotated 'Doc' to a 'Builder' using the given 'Style'.
-- This function is only available with @pretty-1.1.3@ or later.
--
-- /Since: 3/
renderStyleAnnotB :: Style -> Annot.Doc a -> Builder
renderStyleAnnotB sty doc =
    Annot.fullRender (mode sty)
                     (lineLength sty)
                     (ribbonsPerLine sty)
                     txtPrinter
                     mempty
                     doc

-- | Convert an 'AnnotDetais' value to a 'Builder' with the given show function
-- and precedence. This function is only available with @pretty-1.1.3@ or later.
--
-- /Since: 3/
liftShowbAnnotDetailsPrec :: (Int -> a -> Builder) -> Int -> AnnotDetails a -> Builder
liftShowbAnnotDetailsPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbAnnotDetailsPrec #-}

-- | Convert an annotated 'PrettyLevel' value to a 'Builder' with the given precedence.
-- This function is only available with @pretty-1.1.3@ or later.
--
-- /Since: 3/
showbPrettyLevelAnnotPrec :: Int -> Annot.PrettyLevel -> Builder
showbPrettyLevelAnnotPrec = showbPrec
{-# INLINE showbPrettyLevelAnnotPrec #-}

-- | Convert a 'Span' to a 'Builder' with the given show function and precedence.
-- This function is only available with @pretty-1.1.3@ or later.
--
-- /Since: 3/
liftShowbSpanPrec :: (Int -> a -> Builder) -> Int -> Span a -> Builder
liftShowbSpanPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbSpanPrec #-}
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

#if MIN_VERSION_pretty(1,1,3)
$(deriveTextShow  ''AnnotDetails)
$(deriveTextShow1 ''AnnotDetails)

instance TextShow (Annot.Doc a) where
    showb = renderAnnotB
    INLINE_INST_FUN(showb)
instance TextShow1 Annot.Doc where
    liftShowbPrec _ _ = showbPrec
    INLINE_INST_FUN(liftShowbPrec)

$(deriveTextShow ''Annot.PrettyLevel)

$(deriveTextShow  ''Span)
$(deriveTextShow1 ''Span)
#endif
