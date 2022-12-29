{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ == 800
-- See Note [Increased simpl-tick-factor on old GHCs]
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}
#endif

{-|
Module:      TextShow.Data.Binary
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Decoder'.

/Since: 2/
-}
module TextShow.Data.Binary () where

import Data.Binary.Get.Internal (Decoder(..))

import Prelude ()
import Prelude.Compat

import TextShow (TextShow(..), TextShow1(..), Builder, fromString, showbPrec1)

-- | /Since: 2/
instance TextShow a => TextShow (Decoder a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 Decoder where
    liftShowbPrec sp' _ _ = go $ sp' 0
      where
        go :: (a -> Builder) -> Decoder a -> Builder
        go _  (Fail _ msg)    = "Fail: " <> fromString msg
        go _  (Partial _)     = "Partial _"
        go sp (Done _ a)      = "Done: " <> sp a
        go _  (BytesRead _ _) = "BytesRead"
    {-# INLINE liftShowbPrec #-}

{-
Note [Increased simpl-tick-factor on old GHCs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Compiling certain text-show modules with optimizations on old versions of GHC
(particularly 8.0 and 8.2) will trigger "Simplifier ticks exhausted" panics.
To make things worse, this sometimes depends on whether a certain version of
the text library is being used. There are two possible ways to work around
this issue:

1. Figure out which uses of the INLINE pragma in text-show are responsible
   and remove them.
2. Just increase the tick limit.

Since executing on (1) will require a lot of effort to fix an issue that only
happens on old versions of GHC, I've opted for the simple solution of (2) for
now. Issue #51 is a reminder to revisit this choice.
-}
