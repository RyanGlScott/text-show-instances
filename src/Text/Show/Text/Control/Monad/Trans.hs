{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-warnings-deprecations #-}
{-|
Module:      Text.Show.Text.Control.Monad.Trans
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for monad transformers.

/Since: 0.1/
-}
module Text.Show.Text.Control.Monad.Trans (
      showbErrorTPrec
    , showbExceptTPrec
    , showbIdentityTPrec
    , showbListTPrec
    , showbMaybeTPrec
    , showbWriterTLazyPrec
    , showbWriterTStrictPrec
    ) where

import           Control.Monad.Trans.Error               (ErrorT(..))
import           Control.Monad.Trans.Except              (ExceptT(..))
import           Control.Monad.Trans.Identity            (IdentityT(..))
import           Control.Monad.Trans.List                (ListT(..))
import           Control.Monad.Trans.Maybe               (MaybeT(..))
import qualified Control.Monad.Trans.Writer.Lazy   as WL (WriterT(..))
import qualified Control.Monad.Trans.Writer.Strict as WS (WriterT(..))

import           Prelude hiding (Show)

import           Text.Show.Text (Show(showbPrec), Show1(showbPrec1),
                                 Builder, showbUnary1)

#include "inline.h"

-- | Convert an 'ErrorT' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbErrorTPrec :: (Show e, Show1 m, Show a) => Int -> ErrorT e m a -> Builder
showbErrorTPrec p (ErrorT m) = showbUnary1 "ErrorT" p m
{-# INLINE showbErrorTPrec #-}

-- | Convert an 'ExceptT' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbExceptTPrec :: (Show e, Show1 m, Show a) => Int -> ExceptT e m a -> Builder
showbExceptTPrec p (ExceptT m) = showbUnary1 "ExceptT" p m
{-# INLINE showbExceptTPrec #-}

-- | Convert an 'IdentityT' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbIdentityTPrec :: (Show1 f, Show a) => Int -> IdentityT f a -> Builder
showbIdentityTPrec p (IdentityT m) = showbUnary1 "IdentityT" p m
{-# INLINE showbIdentityTPrec #-}

-- | Convert a 'ListT' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbListTPrec :: (Show1 m, Show a) => Int -> ListT m a -> Builder
showbListTPrec p (ListT m) = showbUnary1 "ListT" p m
{-# INLINE showbListTPrec #-}

-- | Convert a 'MaybeT' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbMaybeTPrec :: (Show1 m, Show a) => Int -> MaybeT m a -> Builder
showbMaybeTPrec p (MaybeT m) = showbUnary1 "MaybeT" p m
{-# INLINE showbMaybeTPrec #-}

-- | Convert a lazy 'WL.WriterT' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbWriterTLazyPrec :: (Show w, Show1 m, Show a) => Int -> WL.WriterT w m a -> Builder
showbWriterTLazyPrec p (WL.WriterT m) = showbUnary1 "WriterT" p m
{-# INLINE showbWriterTLazyPrec #-}

-- | Convert a strict 'WS.WriterT' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbWriterTStrictPrec :: (Show w, Show1 m, Show a) => Int -> WS.WriterT w m a -> Builder
showbWriterTStrictPrec p (WS.WriterT m) = showbUnary1 "WriterT" p m
{-# INLINE showbWriterTStrictPrec #-}

instance (Show e, Show1 m, Show a) => Show (ErrorT e m a) where
    showbPrec = showbErrorTPrec
    INLINE_INST_FUN(showbPrec)

instance (Show e, Show1 m) => Show1 (ErrorT e m) where
    showbPrec1 = showbErrorTPrec
    INLINE_INST_FUN(showbPrec1)

instance (Show e, Show1 m, Show a) => Show (ExceptT e m a) where
    showbPrec = showbExceptTPrec
    INLINE_INST_FUN(showbPrec)

instance (Show e, Show1 m) => Show1 (ExceptT e m) where
    showbPrec1 = showbExceptTPrec
    INLINE_INST_FUN(showbPrec1)

instance (Show1 f, Show a) => Show (IdentityT f a) where
    showbPrec = showbIdentityTPrec
    INLINE_INST_FUN(showbPrec)

instance Show1 f => Show1 (IdentityT f) where
    showbPrec1 = showbIdentityTPrec
    INLINE_INST_FUN(showbPrec1)

instance (Show1 m, Show a) => Show (ListT m a) where
    showbPrec = showbListTPrec
    INLINE_INST_FUN(showbPrec)

instance Show1 m => Show1 (ListT m) where
    showbPrec1 = showbListTPrec
    INLINE_INST_FUN(showbPrec1)

instance (Show1 m, Show a) => Show (MaybeT m a) where
    showbPrec = showbMaybeTPrec
    INLINE_INST_FUN(showbPrec)

instance Show1 m => Show1 (MaybeT m) where
    showbPrec1 = showbMaybeTPrec
    INLINE_INST_FUN(showbPrec1)

instance (Show w, Show1 m, Show a) => Show (WL.WriterT w m a) where
    showbPrec = showbWriterTLazyPrec
    INLINE_INST_FUN(showbPrec)

instance (Show w, Show1 m) => Show1 (WL.WriterT w m) where
    showbPrec1 = showbWriterTLazyPrec
    INLINE_INST_FUN(showbPrec1)

instance (Show w, Show1 m, Show a) => Show (WS.WriterT w m a) where
    showbPrec = showbWriterTStrictPrec
    INLINE_INST_FUN(showbPrec)

instance (Show w, Show1 m) => Show1 (WS.WriterT w m) where
    showbPrec1 = showbWriterTStrictPrec
    INLINE_INST_FUN(showbPrec1)
