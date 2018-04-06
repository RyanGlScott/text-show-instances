{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Text.PrettyPrint
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'TextShow' instances for data types in the @pretty@ library.

/Since: 2/
-}
module TextShow.Text.PrettyPrint (
      renderB
    , renderStyleB
#if MIN_VERSION_pretty(1,1,3)
    , renderAnnotB
    , renderStyleAnnotB
#endif
    ) where

import           Prelude ()
import           Prelude.Compat

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
#endif

-- | /Since: 2/
instance TextShow Doc where
    showb = renderB
    {-# INLINE showb #-}

-- | /Since: 2/
$(deriveTextShow ''Mode)
-- | /Since: 2/
$(deriveTextShow ''Style)
-- | /Since: 2/
$(deriveTextShow ''TextDetails)

#if MIN_VERSION_pretty(1,1,2)
-- | Only available with @pretty-1.1.2.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''PrettyLevel)
#endif

#if MIN_VERSION_pretty(1,1,3)
-- | Only available with @pretty-1.1.3@ or later.
--
-- /Since: 3/
$(deriveTextShow  ''AnnotDetails)
-- | Only available with @pretty-1.1.3@ or later.
--
-- /Since: 3/
$(deriveTextShow1 ''AnnotDetails)

-- | Only available with @pretty-1.1.3@ or later.
--
-- /Since: 3/
instance TextShow (Annot.Doc a) where
    showb = renderAnnotB
    {-# INLINE showb #-}
-- | Only available with @pretty-1.1.3@ or later.
--
-- /Since: 3/
instance TextShow1 Annot.Doc where
    liftShowbPrec _ _ = showbPrec
    {-# INLINE liftShowbPrec #-}

-- | Only available with @pretty-1.1.3@ or later.
--
-- /Since: 3/
$(deriveTextShow ''Annot.PrettyLevel)

-- | Only available with @pretty-1.1.3@ or later.
--
-- /Since: 3/
$(deriveTextShow  ''Span)
-- | Only available with @pretty-1.1.3@ or later.
--
-- /Since: 3/
$(deriveTextShow1 ''Span)
#endif
