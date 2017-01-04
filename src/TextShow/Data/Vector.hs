{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Vector
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for @Vector@ types.

/Since: 2/
-}
module TextShow.Data.Vector (
      liftShowbVectorPrec
    , showbVectorGenericPrec
    , liftShowbVectorGenericPrec
    , showbVectorPrimitivePrec
    , showbVectorStorablePrec
    , showbVectorUnboxedPrec
    , showbSizePrec
    ) where

import qualified Data.Vector as B (Vector)
#if MIN_VERSION_vector(0,11,0)
import           Data.Vector.Fusion.Bundle.Size (Size)
#else
import           Data.Vector.Fusion.Stream.Size (Size)
#endif
import qualified Data.Vector.Generic as G (Vector)
import           Data.Vector.Generic (toList)
import qualified Data.Vector.Primitive as P (Vector)
import           Data.Vector.Primitive (Prim)
import qualified Data.Vector.Storable as S (Vector)
import qualified Data.Vector.Unboxed as U (Vector)
import           Data.Vector.Unboxed (Unbox)

import           Foreign.Storable (Storable)

import           TextShow (TextShow(..), TextShow1(..), Builder)
import           TextShow.TH (deriveTextShow)
#if !(MIN_VERSION_vector(0,11,0))
import           TextShow.Utils (showbUnaryList, showbUnaryListWith)
#endif

#include "inline.h"

-- | Convert a boxed 'B.Vector' to a 'Builder' with the given show function
-- and precedence.
-- Note that with @vector-0.11@ and above, the precedence argument is ignored.
--
-- /Since: 3/
liftShowbVectorPrec :: ([a] -> Builder) -> Int -> B.Vector a -> Builder
liftShowbVectorPrec = liftShowbVectorGenericPrec
{-# INLINE liftShowbVectorPrec #-}

-- | Convert a generic 'G.Vector' to a 'Builder' with the given precedence.
-- Note that with @vector-0.11@ and above, the precedence argument is ignored.
--
-- /Since: 2/
showbVectorGenericPrec :: (G.Vector v a, TextShow a) => Int -> v a -> Builder
#if MIN_VERSION_vector(0,11,0)
showbVectorGenericPrec _ = showbList . toList
#else
showbVectorGenericPrec p = showbUnaryListWith showbList p . toList
#endif
{-# INLINE showbVectorGenericPrec #-}

-- | Convert a generic 'G.Vector' to a 'Builder' with the given show function
-- and precedence.
-- Note that with @vector-0.11@ and above, the precedence argument is ignored.
--
-- /Since: 3/
liftShowbVectorGenericPrec :: G.Vector v a => ([a] -> Builder) -> Int -> v a -> Builder
#if MIN_VERSION_vector(0,11,0)
liftShowbVectorGenericPrec sl _ = sl . toList
#else
liftShowbVectorGenericPrec sl p = showbUnaryListWith sl p . toList
#endif
{-# INLINE liftShowbVectorGenericPrec #-}

-- | Convert a primitive 'P.Vector' to a 'Builder' with the given precedence.
-- Note that with @vector-0.11@ and above, the precedence argument is ignored.
--
-- /Since: 2/
showbVectorPrimitivePrec :: (TextShow a, Prim a) => Int -> P.Vector a -> Builder
showbVectorPrimitivePrec = showbVectorGenericPrec
{-# INLINE showbVectorPrimitivePrec #-}

-- | Convert a storable 'S.Vector' to a 'Builder' with the given precedence.
-- Note that with @vector-0.11@ and above, the precedence argument is ignored.
--
-- /Since: 2/
showbVectorStorablePrec :: (TextShow a, Storable a) => Int -> S.Vector a -> Builder
showbVectorStorablePrec = showbVectorGenericPrec
{-# INLINE showbVectorStorablePrec #-}

-- | Convert an unboxed 'U.Vector' to a 'Builder' with the given precedence.
-- Note that with @vector-0.11@ and above, the precedence argument is ignored.
--
-- /Since: 2/
showbVectorUnboxedPrec :: (TextShow a, Unbox a) => Int -> U.Vector a -> Builder
showbVectorUnboxedPrec = showbVectorGenericPrec
{-# INLINE showbVectorUnboxedPrec #-}

-- | Convert a 'Size' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbSizePrec :: Int -> Size -> Builder
showbSizePrec = showbPrec
{-# INLINE showbSizePrec #-}

instance TextShow a => TextShow (B.Vector a) where
    showbPrec = showbVectorGenericPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow1 B.Vector where
    liftShowbPrec _ sl = liftShowbVectorPrec sl
    INLINE_INST_FUN(liftShowbPrec)

instance (TextShow a, Prim a) => TextShow (P.Vector a) where
    showbPrec = showbVectorPrimitivePrec
    INLINE_INST_FUN(showbPrec)

instance (TextShow a, Storable a) => TextShow (S.Vector a) where
    showbPrec = showbVectorStorablePrec
    INLINE_INST_FUN(showbPrec)

instance (TextShow a, Unbox a) => TextShow (U.Vector a) where
    showbPrec = showbVectorUnboxedPrec
    INLINE_INST_FUN(showbPrec)

$(deriveTextShow ''Size)
