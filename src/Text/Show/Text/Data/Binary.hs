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
module Text.Show.Text.Data.Binary (showbDecoderWith) where

import Data.Binary.Get.Internal (Decoder(..))
import Data.Monoid.Compat

import Prelude hiding (Show)

import Text.Show.Text (Show(..), Show1(..), Builder,
                       fromString, showbPrec1)

#include "inline.h"

-- | Convert a 'Decoder' to a 'Builder' with the given show function.
--
-- /Since: 1/
showbDecoderWith :: (a -> Builder) -> Decoder a -> Builder
showbDecoderWith _  (Fail _ msg)    = "Fail: " <> fromString msg
showbDecoderWith _  (Partial _)     = "Partial _"
showbDecoderWith sp (Done _ a)      = "Done: " <> sp a
showbDecoderWith _  (BytesRead _ _) = "BytesRead"

instance Show a => Show (Decoder a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showb)

instance Show1 Decoder where
    showbPrecWith sp _ = showbDecoderWith $ sp 0
    INLINE_INST_FUN(showbPrecWith)
