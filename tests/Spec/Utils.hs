{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module:      Spec.Utils
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ property-related utility functions.
-}
module Spec.Utils (prop_matchesShow, prop_genericShow, prop_genericShow1) where

#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics (Generic, Rep)
# if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic1, Rep1)
# endif
import           Text.Show.Text.Generic
#endif

import           Prelude hiding (Show)

import qualified Text.Show as S (Show)
import           Text.Show.Text as T

-- | Verifies that a type's @Show@ instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesShow :: (S.Show a, T.Show a) => Int -> a -> Bool
prop_matchesShow p x = showbPrec p (FromStringShow x) == showbPrec p x

-- | Verifies that a type's @Show@ instance coincides with the output produced
-- by the equivalent 'Generic' functions.
#if __GLASGOW_HASKELL__ >= 702
prop_genericShow :: (T.Show a, Generic a, GShow (Rep a))
                 => Int -> a -> Bool
prop_genericShow p x = showbPrec p x == genericShowbPrec p x
#else
prop_genericShow :: Int -> a -> Bool
prop_genericShow _ _ = True
#endif

-- | Verifies that a type's @Show1@ instance coincides with the output produced
-- by the equivalent 'Generic1' functions.
#if __GLASGOW_HASKELL__ >= 706
prop_genericShow1 :: (T.Show1 f, Generic1 f, GShow1 (Rep1 f))
                  => Int -> f a -> Bool
prop_genericShow1 p x = showbPrecWith showb27Prec p x
                        == genericShowbPrecWith showb27Prec p x
#else
prop_genericShow1 :: Int -> f a -> Bool
prop_genericShow1 _ _ = True
#endif

#if __GLASGOW_HASKELL__ >= 706
-- | Show the number 27, which certain parody singer-songwriters find humorous.
-- Useful for testing higher-order @Show@ classes.
showb27Prec :: Int -> a -> Builder
showb27Prec p _ = showbPrec p $ Just (27 :: Int)
#endif