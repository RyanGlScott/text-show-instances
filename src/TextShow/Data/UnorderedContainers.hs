{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.UnorderedContainers
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for 'HashMap's and 'HashSet's.

/Since: 2/
-}
module TextShow.Data.UnorderedContainers (
      liftShowbHashMapPrec2
    , liftShowbHashSetPrec
    ) where

import qualified Data.HashMap.Lazy as HM (toList)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashSet as HS (toList)
import           Data.HashSet (HashSet)

import           TextShow (TextShow(..), TextShow1(..), TextShow2(..),
                           Builder, showbPrec1)
import           TextShow.Utils (showbUnaryListWith)

#include "inline.h"

-- | Convert a 'HashMap' to a 'Builder' with the given show functions and precedence.
--
-- /Since: 3/
liftShowbHashMapPrec2 :: (k -> Builder) -> (v -> Builder)
                      -> Int -> HashMap k v -> Builder
liftShowbHashMapPrec2 sp1 sp2 p =
    showbUnaryListWith (liftShowbList2 (const sp1) undefined
                                       (const sp2) undefined) p . HM.toList
{-# INLINE liftShowbHashMapPrec2 #-}

-- | Convert a 'HashSet' to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbHashSetPrec :: ([a] -> Builder) -> Int -> HashSet a -> Builder
liftShowbHashSetPrec sl p = showbUnaryListWith sl p . HS.toList
{-# INLINE liftShowbHashSetPrec #-}

instance (TextShow k, TextShow v) => TextShow (HashMap k v) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow k => TextShow1 (HashMap k) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
    INLINE_INST_FUN(liftShowbPrec)

instance TextShow2 HashMap where
    liftShowbPrec2 sp1 _ sp2 _ = liftShowbHashMapPrec2 (sp1 0) (sp2 0)
    INLINE_INST_FUN(liftShowbPrec2)

instance TextShow a => TextShow (HashSet a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 HashSet where
    liftShowbPrec _ = liftShowbHashSetPrec
    INLINE_INST_FUN(liftShowbPrec)
