{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Binary
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Decoder's.

/Since: 0.2/
-}
module Text.Show.Text.Data.Binary (showbDecoder) where

import Data.Binary.Get.Internal (Decoder(..))
import Data.Monoid.Compat

import Prelude hiding (Show)

import Text.Show.Text (Show(showb, showbPrec), Show1(showbPrec1), Builder, fromString)

#include "inline.h"

-- | Convert a 'Decoder' to a 'Builder'.
-- 
-- /Since: 0.2/
showbDecoder :: Show a => Decoder a -> Builder
showbDecoder (Fail _ msg)    = "Fail: " <> fromString msg
showbDecoder (Partial _)     = "Partial _"
showbDecoder (Done _ a)      = "Done: " <> showb a
showbDecoder (BytesRead _ _) = "BytesRead"

instance Show a => Show (Decoder a) where
    showb = showbDecoder
    INLINE_INST_FUN(showb)

instance Show1 Decoder where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)
