{-# LANGUAGE CPP, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.String.UTF8
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'UTF8' strings.

/Since: 0.2/
-}
module Text.Show.Text.Data.String.UTF8 (showbUTF8, toBuilder, toBuilderGeneric) where

import qualified Codec.Binary.UTF8.Generic as G (foldr)
import           Codec.Binary.UTF8.Generic (UTF8Bytes)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (mempty)
#endif
import           Data.String.UTF8 (UTF8, toRep)
import           Data.Word (Word8)

import           Prelude hiding (Show)

import           Text.Show.Text (Show(showb), Builder)
import           Text.Show.Text.Data.Text (showbBuilder)
import           Text.Show.Text.Utils ((<>), s)

#include "inline.h"

-- | Convert a 'UTF8' string to a 'Builder'.
-- 
-- /Since: 0.2/
showbUTF8 :: UTF8Bytes string index => UTF8 string -> Builder
showbUTF8 = showbBuilder . toBuilder
{-# INLINE showbUTF8 #-}

-- | Convert a 'UTF8'-encoded string to a 'Builder'.
-- Invalid characters are replaced with '\xFFFD'.
-- 
-- /Since: 0.2/
toBuilder :: UTF8Bytes string index => UTF8 string -> Builder
toBuilder = toBuilderGeneric . toRep
{-# INLINE toBuilder #-}

-- | Convert a 'UTF8'-encoded bytestring to a 'Builder'.
-- Invalid characters are replaced with '\xFFFD'.
-- 
-- /Since: 0.2/
toBuilderGeneric :: UTF8Bytes bytestring index => bytestring -> Builder
toBuilderGeneric = G.foldr ((<>) . s) mempty
{-# SPECIALIZE toBuilderGeneric :: BS.ByteString -> Builder #-}
{-# SPECIALIZE toBuilderGeneric :: BL.ByteString -> Builder #-}
{-# SPECIALIZE toBuilderGeneric :: [Word8]       -> Builder #-}

instance UTF8Bytes string index => Show (UTF8 string) where
    showb = showbUTF8
    INLINE_INST_FUN(showb)