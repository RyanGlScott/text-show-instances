{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds         #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Functor.Trans
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for functor transformers.

Note that :

* An instance for 'Identity' is found in @text-show@, since it is a part of
  @base-4.8.0.0@ and later.

* Instances for 'Compose', 'Product', and 'Sum' are found in @text-show@, since they
  are part of @base-4.9.0.0@ and later.

/Since: 2/
-}
module TextShow.Data.Functor.Trans (liftShowbConstantPrec, liftShowbReversePrec) where

import Data.Functor.Constant (Constant(..))
import Data.Functor.Reverse  (Reverse(..))

import TextShow (TextShow(..), TextShow1(..), TextShow2(..),
                 Builder, showbPrec1, showbUnaryWith)

#include "inline.h"

-- | Convert a 'Constant' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 3/
liftShowbConstantPrec :: (Int -> a -> Builder) -> Int -> Constant a b -> Builder
liftShowbConstantPrec sp p (Constant x) = showbUnaryWith sp "Constant" p x
{-# INLINE liftShowbConstantPrec #-}

-- | Convert a 'Reverse' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 3/
liftShowbReversePrec :: TextShow1 f
                     => (Int -> a -> Builder) -> ([a] -> Builder)
                     -> Int -> Reverse f a -> Builder
liftShowbReversePrec sp sl p (Reverse x) =
    showbUnaryWith (liftShowbPrec sp sl) "Reverse" p x
{-# INLINE liftShowbReversePrec #-}

instance TextShow a => TextShow (Constant a b) where
    showbPrec = liftShowbConstantPrec showbPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow a => TextShow1 (Constant a) where
    liftShowbPrec _ _ = liftShowbConstantPrec showbPrec
    INLINE_INST_FUN(liftShowbPrec)

instance TextShow2 Constant where
    liftShowbPrec2 sp _ _ _ = liftShowbConstantPrec sp
    INLINE_INST_FUN(liftShowbPrec2)

instance (TextShow1 f, TextShow a) => TextShow (Reverse f a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 f => TextShow1 (Reverse f) where
    liftShowbPrec = liftShowbReversePrec
    INLINE_INST_FUN(liftShowbPrec)
