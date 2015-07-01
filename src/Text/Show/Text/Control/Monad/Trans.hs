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
      showbErrorTPrecWith
    , showbExceptTPrecWith
    , showbIdentityTPrecWith
    , showbListTPrec
    , showbListTPrecWith
    , showbMaybeTPrecWith
    , showbWriterTLazyPrecWith
    , showbWriterTStrictPrecWith
    ) where

import           Control.Monad.Trans.Error               (ErrorT(..))
import           Control.Monad.Trans.Except              (ExceptT(..))
import           Control.Monad.Trans.Identity            (IdentityT(..))
import           Control.Monad.Trans.List                (ListT(..))
import           Control.Monad.Trans.Maybe               (MaybeT(..))
import qualified Control.Monad.Trans.Writer.Lazy   as WL (WriterT(..))
import qualified Control.Monad.Trans.Writer.Strict as WS (WriterT(..))

import           Prelude hiding (Show)

import           Text.Show.Text (Show(showb, showbPrec), Show1(..), Builder,
                                 showbPrec1, showbUnaryWith)
import           Text.Show.Text.Data.Tuple (showb2TupleWith2)

#include "inline.h"

-- | Convert an 'ErrorT' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 1/
showbErrorTPrecWith :: (Show e, Show1 m)
                    => (Int -> a -> Builder)
                    -> Int -> ErrorT e m a -> Builder
showbErrorTPrecWith sp p (ErrorT m) =
    showbUnaryWith (showbPrecWith (showbPrecWith sp)) "ErrorT" p m
{-# INLINE showbErrorTPrecWith #-}

-- | Convert an 'ExceptT' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 1/
showbExceptTPrecWith :: (Show e, Show1 m)
                     => (Int -> a -> Builder)
                     -> Int -> ExceptT e m a -> Builder
showbExceptTPrecWith sp p (ExceptT m) =
    showbUnaryWith (showbPrecWith (showbPrecWith sp)) "ExceptT" p m
{-# INLINE showbExceptTPrecWith #-}

-- | Convert an 'IdentityT' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 1/
showbIdentityTPrecWith :: Show1 f
                       => (Int -> a -> Builder)
                       -> Int -> IdentityT f a -> Builder
showbIdentityTPrecWith sp p (IdentityT m) =
    showbUnaryWith (showbPrecWith sp) "IdentityT" p m
{-# INLINE showbIdentityTPrecWith #-}

-- | Convert a 'ListT' value to a 'Builder' with the given precedence.
--
-- /Since: 0.1/
showbListTPrec :: (Show1 m, Show a) => Int -> ListT m a -> Builder
showbListTPrec p (ListT m) = showbUnaryWith (showbPrecWith showbPrec) "ListT" p m
{-# INLINE showbListTPrec #-}

-- | Convert a 'ListT' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbListTPrecWith :: Show1 m
                   => (a -> Builder)
                   -> Int -> ListT m a -> Builder
showbListTPrecWith sp p (ListT m) =
    showbUnaryWith (showbPrecWith (showbPrecWith (const sp))) "ListT" p m
{-# INLINE showbListTPrecWith #-}

-- | Convert a 'MaybeT' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 1/
showbMaybeTPrecWith :: Show1 m
                    => (Int -> a -> Builder)
                    -> Int -> MaybeT m a -> Builder
showbMaybeTPrecWith sp p (MaybeT m) =
    showbUnaryWith (showbPrecWith (showbPrecWith sp)) "MaybeT" p m
{-# INLINE showbMaybeTPrecWith #-}

-- | Convert a lazy 'WL.WriterT' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 1/
showbWriterTLazyPrecWith :: (Show w, Show1 m)
                         => (a -> Builder)
                         -> Int -> WL.WriterT w m a -> Builder
showbWriterTLazyPrecWith sp p (WL.WriterT m) =
    showbUnaryWith (showbPrecWith (const $ showb2TupleWith2 sp showb)) "WriterT" p m
{-# INLINE showbWriterTLazyPrecWith #-}

-- | Convert a strict 'WS.WriterT' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 1/
showbWriterTStrictPrecWith :: (Show w, Show1 m)
                           => (a -> Builder)
                           -> Int -> WS.WriterT w m a -> Builder
showbWriterTStrictPrecWith sp p (WS.WriterT m) =
    showbUnaryWith (showbPrecWith (const $ showb2TupleWith2 sp showb)) "WriterT" p m
{-# INLINE showbWriterTStrictPrecWith #-}

instance (Show e, Show1 m, Show a) => Show (ErrorT e m a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance (Show e, Show1 m) => Show1 (ErrorT e m) where
    showbPrecWith = showbErrorTPrecWith
    INLINE_INST_FUN(showbPrecWith)

instance (Show e, Show1 m, Show a) => Show (ExceptT e m a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance (Show e, Show1 m) => Show1 (ExceptT e m) where
    showbPrecWith = showbExceptTPrecWith
    INLINE_INST_FUN(showbPrecWith)

instance (Show1 f, Show a) => Show (IdentityT f a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance Show1 f => Show1 (IdentityT f) where
    showbPrecWith = showbIdentityTPrecWith
    INLINE_INST_FUN(showbPrecWith)

instance (Show1 m, Show a) => Show (ListT m a) where
    showbPrec = showbListTPrec
    INLINE_INST_FUN(showbPrec)

instance Show1 m => Show1 (ListT m) where
    showbPrecWith sp = showbListTPrecWith $ sp 0
    INLINE_INST_FUN(showbPrecWith)

instance (Show1 m, Show a) => Show (MaybeT m a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance Show1 m => Show1 (MaybeT m) where
    showbPrecWith = showbMaybeTPrecWith
    INLINE_INST_FUN(showbPrecWith)

instance (Show w, Show1 m, Show a) => Show (WL.WriterT w m a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance (Show w, Show1 m) => Show1 (WL.WriterT w m) where
    showbPrecWith sp = showbWriterTLazyPrecWith $ sp 0
    INLINE_INST_FUN(showbPrecWith)

instance (Show w, Show1 m, Show a) => Show (WS.WriterT w m a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance (Show w, Show1 m) => Show1 (WS.WriterT w m) where
    showbPrecWith sp = showbWriterTStrictPrecWith $ sp 0
    INLINE_INST_FUN(showbPrecWith)
