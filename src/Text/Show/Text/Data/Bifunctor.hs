{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Text.Show.Text.Data.Bifunctor
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @bifunctors@ library.

/Since: 1/
-}
module Text.Show.Text.Data.Bifunctor (
      showbBiffPrec
    , showbBiffPrecWith2
    , showbClownPrec
    , showbClownPrecWith
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
import Data.Bifunctor.Flip (Flip)
import Data.Bifunctor.Join (Join(..))
import Data.Bifunctor.Joker (Joker)
import Data.Bifunctor.Product (Product)
import Data.Bifunctor.Tannen (Tannen)
import Data.Bifunctor.Wrapped (WrappedBifunctor)

import Prelude hiding (Show)

import Text.Show.Text (Show(..), Show1(..), Show2(..), Builder)
import Text.Show.Text.TH (deriveShow1, deriveShow2, mkShowbPrec, mkShowbPrecWith)

-- | Convert a 'Biff' value to a 'Builder' with the given precedence.
--
-- /Since: 1/
showbBiffPrec :: Show (p (f a) (g b)) => Int -> Biff p f g a b -> Builder
showbBiffPrec = showbPrec
{-# INLINE showbBiffPrec #-}

-- | Convert a 'Biff' value to a 'Builder' with the given show functions and precedence.
--
-- /Since: 1/
showbBiffPrecWith2 :: (Show2 p, Show1 f, Show1 g)
                   => (Int -> a -> Builder) -> (Int -> b -> Builder)
                   -> Int -> Biff p f g a b -> Builder
showbBiffPrecWith2 = showbPrecWith2
{-# INLINE showbBiffPrecWith2 #-}

-- | Convert a 'Clown' value to a 'Builder' with the given precedence.
--
-- /Since: 1/
showbClownPrec :: Show (f a) => Int -> Clown f a b -> Builder
showbClownPrec = showbPrec
{-# INLINE showbClownPrec #-}

-- | Convert a 'Clown' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbClownPrecWith :: Show1 f => (Int -> a -> Builder) -> Int -> Clown f a b -> Builder
showbClownPrecWith sp = showbPrecWith2 sp undefined
{-# INLINE showbClownPrecWith #-}

-- | Convert a 'Flip' value to a 'Builder' with the given precedence.
--
-- /Since: 1/
showbFlipPrec :: Show (p b a) => Int -> Flip p a b -> Builder
showbFlipPrec = showbPrec
{-# INLINE showbFlipPrec #-}

-- | Convert a 'Flip' value to a 'Builder' with the given show functions and precedence.
--
-- /Since: 1/
showbFlipPrecWith2 :: Show2 p
                   => (Int -> a -> Builder) -> (Int -> b -> Builder)
                   -> Int -> Flip p a b -> Builder
showbFlipPrecWith2 = showbPrecWith2
{-# INLINE showbFlipPrecWith2 #-}

-- | Convert a 'Join' value to a 'Builder' with the given precedence.
--
-- /Since: 1/
showbJoinPrec :: Show (p a a) => Int -> Join p a -> Builder
showbJoinPrec = showbPrec
{-# INLINE showbJoinPrec #-}

-- | Convert a 'Join' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbJoinPrecWith :: Show2 p => (Int -> a -> Builder) -> Int -> Join p a -> Builder
showbJoinPrecWith sp p = showbPrecWith2 sp sp p . runJoin
{-# INLINE showbJoinPrecWith #-}

-- | Convert a 'Joker' value to a 'Builder' with the given precedence.
--
-- /Since: 1/
showbJokerPrec :: Show (g b) => Int -> Joker g a b -> Builder
showbJokerPrec = showbPrec
{-# INLINE showbJokerPrec #-}

-- | Convert a 'Joker' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbJokerPrecWith :: Show1 g => (Int -> b -> Builder) -> Int -> Joker g a b -> Builder
showbJokerPrecWith sp = showbPrecWith2 undefined sp
{-# INLINE showbJokerPrecWith #-}

-- | Convert a 'Product' value to a 'Builder' with the given precedence.
--
-- /Since: 1/
showbProductPrec :: (Show (f a b), Show (g a b)) => Int -> Product f g a b -> Builder
showbProductPrec = showbPrec
{-# INLINE showbProductPrec #-}

-- | Convert a 'Product' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 1/
showbProductPrecWith2 :: (Show2 f, Show2 g)
                      => (Int -> a -> Builder) -> (Int -> b -> Builder)
                      -> Int -> Product f g a b -> Builder
showbProductPrecWith2 = showbPrecWith2
{-# INLINE showbProductPrecWith2 #-}

-- | Convert a 'Tannen' value to a 'Builder' with the given precedence.
--
-- /Since: 1/
showbTannenPrec :: Show (f (p a b)) => Int -> Tannen f p a b -> Builder
showbTannenPrec = showbPrec
{-# INLINE showbTannenPrec #-}

-- | Convert a 'Tannen' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 1/
showbTannenPrecWith2 :: (Show1 f, Show2 p)
                     => (Int -> a -> Builder) -> (Int -> b -> Builder)
                     -> Int -> Tannen f p a b -> Builder
showbTannenPrecWith2 = showbPrecWith2
{-# INLINE showbTannenPrecWith2 #-}

-- | Convert a 'WrappedBifunctor' value to a 'Builder' with the given precedence.
--
-- /Since: 1/
showbWrappedBifunctorPrec :: Show (p a b) => Int -> WrappedBifunctor p a b -> Builder
showbWrappedBifunctorPrec = showbPrec
{-# INLINE showbWrappedBifunctorPrec #-}

-- | Convert a 'WrappedBifunctor' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 1/
showbWrappedBifunctorPrecWith2 :: Show2 p
                               => (Int -> a -> Builder) -> (Int -> b -> Builder)
                               -> Int -> WrappedBifunctor p a b -> Builder
showbWrappedBifunctorPrecWith2 = showbPrecWith2
{-# INLINE showbWrappedBifunctorPrecWith2 #-}

instance Show (p (f a) (g b)) => Show (Biff p f g a b) where
    showbPrec = $(mkShowbPrec ''Biff)
instance (Show2 p, Show1 f, Show1 g, Show a) => Show1 (Biff p f g a) where
    showbPrecWith = showbPrecWith2 showbPrec
$(deriveShow2 ''Biff)

instance Show (f a) => Show (Clown f a b) where
    showbPrec = $(mkShowbPrec ''Clown)
$(deriveShow1 ''Clown)
$(deriveShow2 ''Clown)

instance Show (p b a) => Show (Flip p a b) where
    showbPrec = $(mkShowbPrec ''Flip)
instance (Show2 p, Show a) => Show1 (Flip p a) where
    showbPrecWith = showbPrecWith2 showbPrec
$(deriveShow2 ''Flip)

instance Show (p a a) => Show (Join p a) where
    showbPrec = $(mkShowbPrec ''Join)
instance Show2 p => Show1 (Join p) where
    showbPrecWith = showbJoinPrecWith

instance Show (g b) => Show (Joker g a b) where
    showbPrec = $(mkShowbPrec ''Joker)
instance Show1 g => Show1 (Joker g a) where
    showbPrecWith = $(mkShowbPrecWith ''Joker)
$(deriveShow2 ''Joker)

instance (Show (f a b), Show (g a b)) => Show (Product f g a b) where
    showbPrec = $(mkShowbPrec ''Product)
instance (Show2 f, Show2 g, Show a) => Show1 (Product f g a) where
    showbPrecWith = showbPrecWith2 showbPrec
$(deriveShow2 ''Product)

instance Show (f (p a b)) => Show (Tannen f p a b) where
    showbPrec = $(mkShowbPrec ''Tannen)
instance (Show1 f, Show2 p, Show a) => Show1 (Tannen f p a) where
    showbPrecWith = showbPrecWith2 showbPrec
$(deriveShow2 ''Tannen)

instance Show (p a b) => Show (WrappedBifunctor p a b) where
    showbPrec = $(mkShowbPrec ''WrappedBifunctor)
instance (Show2 p, Show a) => Show1 (WrappedBifunctor p a) where
    showbPrecWith = showbPrecWith2 showbPrec
$(deriveShow2 ''WrappedBifunctor)
