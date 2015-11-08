{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Bifunctor
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @bifunctors@ library.

/Since: 2/
-}
module TextShow.Data.Bifunctor (
      showbBiffPrec
    , showbBiffPrecWith2
    , showbClownPrec
    , showbClownPrecWith
    , showbFixPrec
    , showbFixPrecWith
    , showbFlipPrec
    , showbFlipPrecWith2
    , showbJoinPrec
    , showbJoinPrecWith
    , showbJokerPrec
    , showbJokerPrecWith
    , showbProductPrec
    , showbProductPrecWith2
    , showbTannenPrec
    , showbTannenPrecWith2
    , showbWrappedBifunctorPrec
    , showbWrappedBifunctorPrecWith2
    ) where

import Data.Bifunctor.Biff (Biff)
import Data.Bifunctor.Clown (Clown)
import Data.Bifunctor.Fix (Fix(..))
import Data.Bifunctor.Flip (Flip)
import Data.Bifunctor.Join (Join(..))
import Data.Bifunctor.Joker (Joker)
import Data.Bifunctor.Product (Product)
import Data.Bifunctor.Tannen (Tannen)
import Data.Bifunctor.Wrapped (WrappedBifunctor)

import TextShow (TextShow(..), TextShow1(..), TextShow2(..), Builder)
import TextShow.TH (deriveTextShow1, deriveTextShow2, makeShowbPrec, makeShowbPrecWith)

-- | Convert a 'Biff' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbBiffPrec :: TextShow (p (f a) (g b)) => Int -> Biff p f g a b -> Builder
showbBiffPrec = showbPrec
{-# INLINE showbBiffPrec #-}

-- | Convert a 'Biff' value to a 'Builder' with the given show functions and precedence.
--
-- /Since: 2/
showbBiffPrecWith2 :: (TextShow2 p, TextShow1 f, TextShow1 g)
                   => (Int -> a -> Builder) -> (Int -> b -> Builder)
                   -> Int -> Biff p f g a b -> Builder
showbBiffPrecWith2 = showbPrecWith2
{-# INLINE showbBiffPrecWith2 #-}

-- | Convert a 'Clown' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbClownPrec :: TextShow (f a) => Int -> Clown f a b -> Builder
showbClownPrec = showbPrec
{-# INLINE showbClownPrec #-}

-- | Convert a 'Clown' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbClownPrecWith :: TextShow1 f => (Int -> a -> Builder) -> Int -> Clown f a b -> Builder
showbClownPrecWith sp = showbPrecWith2 sp undefined
{-# INLINE showbClownPrecWith #-}

-- | Convert a 'Fix' value to a 'Builder' with the given precedence.
--
-- /Since: 3/
showbFixPrec :: TextShow (p (Fix p a) a) => Int -> Fix p a -> Builder
showbFixPrec = showbPrec
{-# INLINE showbFixPrec #-}

-- | Convert a 'Fix' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
showbFixPrecWith :: TextShow2 p => (Int -> a -> Builder) -> Int -> Fix p a -> Builder
showbFixPrecWith sp p = showbPrecWith2 (showbPrecWith sp) sp p . out
{-# INLINE showbFixPrecWith #-}

-- | Convert a 'Flip' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbFlipPrec :: TextShow (p b a) => Int -> Flip p a b -> Builder
showbFlipPrec = showbPrec
{-# INLINE showbFlipPrec #-}

-- | Convert a 'Flip' value to a 'Builder' with the given show functions and precedence.
--
-- /Since: 2/
showbFlipPrecWith2 :: TextShow2 p
                   => (Int -> a -> Builder) -> (Int -> b -> Builder)
                   -> Int -> Flip p a b -> Builder
showbFlipPrecWith2 = showbPrecWith2
{-# INLINE showbFlipPrecWith2 #-}

-- | Convert a 'Join' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbJoinPrec :: TextShow (p a a) => Int -> Join p a -> Builder
showbJoinPrec = showbPrec
{-# INLINE showbJoinPrec #-}

-- | Convert a 'Join' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbJoinPrecWith :: TextShow2 p => (Int -> a -> Builder) -> Int -> Join p a -> Builder
showbJoinPrecWith sp p = showbPrecWith2 sp sp p . runJoin
{-# INLINE showbJoinPrecWith #-}

-- | Convert a 'Joker' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbJokerPrec :: TextShow (g b) => Int -> Joker g a b -> Builder
showbJokerPrec = showbPrec
{-# INLINE showbJokerPrec #-}

-- | Convert a 'Joker' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbJokerPrecWith :: TextShow1 g => (Int -> b -> Builder) -> Int -> Joker g a b -> Builder
showbJokerPrecWith sp = showbPrecWith2 undefined sp
{-# INLINE showbJokerPrecWith #-}

-- | Convert a 'Product' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbProductPrec :: (TextShow (f a b), TextShow (g a b)) => Int -> Product f g a b -> Builder
showbProductPrec = showbPrec
{-# INLINE showbProductPrec #-}

-- | Convert a 'Product' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 2/
showbProductPrecWith2 :: (TextShow2 f, TextShow2 g)
                      => (Int -> a -> Builder) -> (Int -> b -> Builder)
                      -> Int -> Product f g a b -> Builder
showbProductPrecWith2 = showbPrecWith2
{-# INLINE showbProductPrecWith2 #-}

-- | Convert a 'Tannen' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbTannenPrec :: TextShow (f (p a b)) => Int -> Tannen f p a b -> Builder
showbTannenPrec = showbPrec
{-# INLINE showbTannenPrec #-}

-- | Convert a 'Tannen' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 2/
showbTannenPrecWith2 :: (TextShow1 f, TextShow2 p)
                     => (Int -> a -> Builder) -> (Int -> b -> Builder)
                     -> Int -> Tannen f p a b -> Builder
showbTannenPrecWith2 = showbPrecWith2
{-# INLINE showbTannenPrecWith2 #-}

-- | Convert a 'WrappedBifunctor' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbWrappedBifunctorPrec :: TextShow (p a b) => Int -> WrappedBifunctor p a b -> Builder
showbWrappedBifunctorPrec = showbPrec
{-# INLINE showbWrappedBifunctorPrec #-}

-- | Convert a 'WrappedBifunctor' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 2/
showbWrappedBifunctorPrecWith2 :: TextShow2 p
                               => (Int -> a -> Builder) -> (Int -> b -> Builder)
                               -> Int -> WrappedBifunctor p a b -> Builder
showbWrappedBifunctorPrecWith2 = showbPrecWith2
{-# INLINE showbWrappedBifunctorPrecWith2 #-}

instance TextShow (p (f a) (g b)) => TextShow (Biff p f g a b) where
    showbPrec = $(makeShowbPrec ''Biff)
instance (TextShow2 p, TextShow1 f, TextShow1 g, TextShow a) => TextShow1 (Biff p f g a) where
    showbPrecWith = showbPrecWith2 showbPrec
$(deriveTextShow2 ''Biff)

instance TextShow (f a) => TextShow (Clown f a b) where
    showbPrec = $(makeShowbPrec ''Clown)
$(deriveTextShow1 ''Clown)
$(deriveTextShow2 ''Clown)

instance TextShow (p (Fix p a) a) => TextShow (Fix p a) where
    showbPrec = $(makeShowbPrec ''Fix)
instance TextShow2 p => TextShow1 (Fix p) where
    showbPrecWith = showbFixPrecWith

instance TextShow (p b a) => TextShow (Flip p a b) where
    showbPrec = $(makeShowbPrec ''Flip)
instance (TextShow2 p, TextShow a) => TextShow1 (Flip p a) where
    showbPrecWith = showbPrecWith2 showbPrec
$(deriveTextShow2 ''Flip)

instance TextShow (p a a) => TextShow (Join p a) where
    showbPrec = $(makeShowbPrec ''Join)
instance TextShow2 p => TextShow1 (Join p) where
    showbPrecWith = showbJoinPrecWith

instance TextShow (g b) => TextShow (Joker g a b) where
    showbPrec = $(makeShowbPrec ''Joker)
instance TextShow1 g => TextShow1 (Joker g a) where
    showbPrecWith = $(makeShowbPrecWith ''Joker)
$(deriveTextShow2 ''Joker)

instance (TextShow (f a b), TextShow (g a b)) => TextShow (Product f g a b) where
    showbPrec = $(makeShowbPrec ''Product)
instance (TextShow2 f, TextShow2 g, TextShow a) => TextShow1 (Product f g a) where
    showbPrecWith = showbPrecWith2 showbPrec
$(deriveTextShow2 ''Product)

instance TextShow (f (p a b)) => TextShow (Tannen f p a b) where
    showbPrec = $(makeShowbPrec ''Tannen)
instance (TextShow1 f, TextShow2 p, TextShow a) => TextShow1 (Tannen f p a) where
    showbPrecWith = showbPrecWith2 showbPrec
$(deriveTextShow2 ''Tannen)

instance TextShow (p a b) => TextShow (WrappedBifunctor p a b) where
    showbPrec = $(makeShowbPrec ''WrappedBifunctor)
instance (TextShow2 p, TextShow a) => TextShow1 (WrappedBifunctor p a) where
    showbPrecWith = showbPrecWith2 showbPrec
$(deriveTextShow2 ''WrappedBifunctor)
