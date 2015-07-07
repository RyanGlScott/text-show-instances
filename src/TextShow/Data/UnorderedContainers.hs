{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.UnorderedContainers
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for 'HashMap's and 'HashSet's.

/Since: 2/
-}
module TextShow.Data.UnorderedContainers (
      showbHashMapPrecWith2
    , showbHashSetPrec
    , showbHashSetPrecWith
    ) where

import qualified Data.HashMap.Lazy as HM (toList)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashSet as HS (toList)
import           Data.HashSet (HashSet)

import           TextShow (TextShow(showbPrec), TextShow1(..), TextShow2(..),
                           Builder, showbPrec1)
import           TextShow.Data.Tuple (showb2TupleWith2)
import           TextShow.Utils (showbUnaryList, showbUnaryListWith)

#include "inline.h"

-- | Convert a 'HashMap' to a 'Builder' with the given show functions and precedence.
--
-- /Since: 2/
showbHashMapPrecWith2 :: (k -> Builder) -> (v -> Builder)
                      -> Int -> HashMap k v -> Builder
showbHashMapPrecWith2 sp1 sp2 p =
    showbUnaryListWith (showb2TupleWith2 sp1 sp2) p . HM.toList
{-# INLINE showbHashMapPrecWith2 #-}

-- | Convert a 'HashSet' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbHashSetPrec :: TextShow a => Int -> HashSet a -> Builder
showbHashSetPrec p = showbUnaryList p . HS.toList
{-# INLINE showbHashSetPrec #-}

-- | Convert a 'HashSet' to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbHashSetPrecWith :: (a -> Builder) -> Int -> HashSet a -> Builder
showbHashSetPrecWith sp p = showbUnaryListWith sp p . HS.toList
{-# INLINE showbHashSetPrecWith #-}

instance (TextShow k, TextShow v) => TextShow (HashMap k v) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow k => TextShow1 (HashMap k) where
    showbPrecWith = showbPrecWith2 showbPrec
    INLINE_INST_FUN(showbPrecWith)

instance TextShow2 HashMap where
    showbPrecWith2 sp1 sp2 = showbHashMapPrecWith2 (sp1 0) (sp2 0)
    INLINE_INST_FUN(showbPrecWith2)

instance TextShow a => TextShow (HashSet a) where
    showbPrec = showbHashSetPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow1 HashSet where
    showbPrecWith sp = showbHashSetPrecWith $ sp 0
    INLINE_INST_FUN(showbPrecWith)
