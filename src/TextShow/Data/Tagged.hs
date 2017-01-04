{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds         #-}
#endif

{-# OPTIONS -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Tagged
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Tagged' values.

/Since: 2/
-}
module TextShow.Data.Tagged (liftShowbTaggedPrec) where

import Data.Tagged (Tagged(..))
import TextShow (TextShow(..), TextShow1(..), TextShow2(..),
                 Builder, showbPrec1, showbUnaryWith)

#include "inline.h"

-- | Convert a 'Tagged' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbTaggedPrec :: (Int -> b -> Builder) -> Int -> Tagged s b -> Builder
liftShowbTaggedPrec sp p (Tagged b) = showbUnaryWith sp "Tagged" p b
{-# INLINE liftShowbTaggedPrec #-}

instance TextShow b => TextShow (Tagged s b) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 (Tagged s) where
    liftShowbPrec sp _ = liftShowbTaggedPrec sp
    INLINE_INST_FUN(liftShowbPrec)

instance TextShow2 Tagged where
    liftShowbPrec2 _ _ sp _ = liftShowbTaggedPrec sp
    INLINE_INST_FUN(liftShowbPrec2)
