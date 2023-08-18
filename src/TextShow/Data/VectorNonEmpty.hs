{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Data.Vector.NonEmpty
Copyright:   (C) 2014-2023 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for @NonEmptyVector@ types.
-}
module TextShow.Data.VectorNonEmpty () where

import qualified Data.Vector.NonEmpty as B (NonEmptyVector, toList)

import           TextShow (TextShow(..), TextShow1(..), showbPrec1)
import           TextShow.Utils (showbUnaryListWith)

instance TextShow a => TextShow (B.NonEmptyVector a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

instance TextShow1 B.NonEmptyVector where
    liftShowbPrec _ sl p = showbUnaryListWith sl p . B.toList
    {-# INLINE liftShowbPrec #-}

