{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module:      Spec.Utils
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@QuickCheck@ property-related utility functions.
-}
module Spec.Utils (
      prop_matchesTextShow
    , prop_genericTextShow
    , prop_genericTextShow1
    ) where

#if __GLASGOW_HASKELL__ >= 704
import GHC.Generics (Generic, Rep)
# if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1, Rep1)
# endif
import TextShow.Generic
#endif

import TextShow (TextShow(..), TextShow1(..), Builder, FromStringShow(..))

-- | Verifies that a type's @Show@ instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesTextShow :: (Show a, TextShow a) => Int -> a -> Bool
prop_matchesTextShow p x = showbPrec p (FromStringShow x) == showbPrec p x

-- | Verifies that a type's 'TextShow' instance coincides with the output produced
-- by the equivalent 'Generic' functions.
#if __GLASGOW_HASKELL__ >= 704
prop_genericTextShow :: (TextShow a, Generic a, GTextShow (Rep a))
                     => Int -> a -> Bool
prop_genericTextShow p x = showbPrec p x == genericShowbPrec p x
#else
prop_genericTextShow :: Int -> a -> Bool
prop_genericTextShow _ _ = True
#endif

-- | Verifies that a type's 'TextShow1' instance coincides with the output produced
-- by the equivalent 'Generic1' functions.
#if __GLASGOW_HASKELL__ >= 706
prop_genericTextShow1 :: (TextShow1 f, Generic1 f, GTextShow1 (Rep1 f))
                      => Int -> f a -> Bool
prop_genericTextShow1 p x = showbPrecWith showb27Prec p x
                            == genericShowbPrecWith showb27Prec p x
#else
prop_genericTextShow1 :: Int -> f a -> Bool
prop_genericTextShow1 _ _ = True
#endif

#if __GLASGOW_HASKELL__ >= 706
-- | Show the number 27, which certain parody singer-songwriters find humorous.
-- Useful for testing higher-order 'TextShow' classes.
showb27Prec :: Int -> a -> Builder
showb27Prec p _ = showbPrec p $ Just (27 :: Int)
#endif
