{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Binary
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Decoder's.

/Since: 2/
-}
module TextShow.Data.Binary (liftShowbDecoder) where

import Data.Binary.Get.Internal (Decoder(..))
import Data.Monoid.Compat

import TextShow (TextShow(..), TextShow1(..), Builder, fromString, showbPrec1)

#include "inline.h"

-- | Convert a 'Decoder' to a 'Builder' with the given show function.
--
-- /Since: 3/
liftShowbDecoder :: (a -> Builder) -> Decoder a -> Builder
liftShowbDecoder _  (Fail _ msg)    = "Fail: " <> fromString msg
liftShowbDecoder _  (Partial _)     = "Partial _"
liftShowbDecoder sp (Done _ a)      = "Done: " <> sp a
liftShowbDecoder _  (BytesRead _ _) = "BytesRead"

instance TextShow a => TextShow (Decoder a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 Decoder where
    liftShowbPrec sp _ _ = liftShowbDecoder $ sp 0
    INLINE_INST_FUN(liftShowbPrec)
