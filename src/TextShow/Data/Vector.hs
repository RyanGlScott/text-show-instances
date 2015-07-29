{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Vector
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for @Vector@ types.

/Since: 2/
-}
module TextShow.Data.Vector (
      showbVectorPrec
    , showbVectorPrecWith
    , showbVectorGenericPrec
    , showbVectorGenericPrecWith
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

-- | Convert a boxed 'B.Vector' to a 'Builder' with the given precedence.
-- Note that with @vector-0.11@ and above, the precedence argument is ignored.
--
-- /Since: 2/
showbVectorPrec :: TextShow a => Int -> B.Vector a -> Builder
showbVectorPrec = showbVectorGenericPrec
{-# INLINE showbVectorPrec #-}

-- | Convert a boxed 'B.Vector' to a 'Builder' with the given show function
-- and precedence.
-- Note that with @vector-0.11@ and above, the precedence argument is ignored.
--
-- /Since: 2/
showbVectorPrecWith :: (a -> Builder) -> Int -> B.Vector a -> Builder
showbVectorPrecWith = showbVectorGenericPrecWith
{-# INLINE showbVectorPrecWith #-}

-- | Convert a generic 'G.Vector' to a 'Builder' with the given precedence.
-- Note that with @vector-0.11@ and above, the precedence argument is ignored.
--
-- /Since: 2/
showbVectorGenericPrec :: (G.Vector v a, TextShow a) => Int -> v a -> Builder
#if MIN_VERSION_vector(0,11,0)
showbVectorGenericPrec _ = showb . toList
#else
showbVectorGenericPrec p = showbUnaryList p . toList
#endif
{-# INLINE showbVectorGenericPrec #-}

-- | Convert a generic 'G.Vector' to a 'Builder' with the given show function
-- and precedence.
-- Note that with @vector-0.11@ and above, the precedence argument is ignored.
--
-- /Since: 2/
showbVectorGenericPrecWith :: G.Vector v a => (a -> Builder) -> Int -> v a -> Builder
#if MIN_VERSION_vector(0,11,0)
showbVectorGenericPrecWith sp p = showbPrecWith (const sp) p . toList
#else
showbVectorGenericPrecWith sp p = showbUnaryListWith sp p . toList
#endif
{-# INLINE showbVectorGenericPrecWith #-}

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
    showbPrec = showbVectorPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow1 B.Vector where
    showbPrecWith sp = showbVectorPrecWith $ sp 0
    INLINE_INST_FUN(showbPrecWith)

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