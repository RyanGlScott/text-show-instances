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

import qualified Data.Vector.NonEmpty as B (NonEmptyVector, toVector)

import           TextShow.Data.Vector ()

import           TextShow (TextShow(..), TextShow1(..), showbPrec1)

instance TextShow a => TextShow (B.NonEmptyVector a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

instance TextShow1 B.NonEmptyVector where
    liftShowbPrec sp sl p = liftShowbPrec sp sl p . B.toVector
    {-# INLINE liftShowbPrec #-}
