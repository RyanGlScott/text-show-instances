{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-warnings-deprecations #-}
{-|
Module:      TextShow.Control.Monad.Trans
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for monad transformers.

/Since: 2/
-}
module TextShow.Control.Monad.Trans (
      liftShowbErrorTPrec
    , liftShowbExceptTPrec
    , liftShowbIdentityTPrec
    , liftShowbListTPrec
    , liftShowbMaybeTPrec
    , liftShowbWriterTLazyPrec
    , liftShowbWriterTStrictPrec
    ) where

import           Control.Monad.Trans.Error               (ErrorT(..))
import           Control.Monad.Trans.Except              (ExceptT(..))
import           Control.Monad.Trans.Identity            (IdentityT(..))
import           Control.Monad.Trans.List                (ListT(..))
import           Control.Monad.Trans.Maybe               (MaybeT(..))
import qualified Control.Monad.Trans.Writer.Lazy   as WL (WriterT(..))
import qualified Control.Monad.Trans.Writer.Strict as WS (WriterT(..))

import           TextShow (TextShow(..), TextShow1(..), TextShow2(..),
                           Builder, showbPrec1, showbUnaryWith)

#include "inline.h"

-- | Convert an 'ErrorT' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 3/
liftShowbErrorTPrec :: (TextShow e, TextShow1 m)
                    => (Int -> a -> Builder) -> ([a] -> Builder)
                    -> Int -> ErrorT e m a -> Builder
liftShowbErrorTPrec sp sl p (ErrorT m) =
    showbUnaryWith (liftShowbPrec (liftShowbPrec sp sl)
                                  (liftShowbList sp sl)) "ErrorT" p m
{-# INLINE liftShowbErrorTPrec #-}

-- | Convert an 'ExceptT' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 3/
liftShowbExceptTPrec :: (TextShow e, TextShow1 m)
                     => (Int -> a -> Builder) -> ([a] -> Builder)
                     -> Int -> ExceptT e m a -> Builder
liftShowbExceptTPrec sp sl p (ExceptT m) =
    showbUnaryWith (liftShowbPrec (liftShowbPrec sp sl)
                                  (liftShowbList sp sl)) "ExceptT" p m
{-# INLINE liftShowbExceptTPrec #-}

-- | Convert an 'IdentityT' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 3/
liftShowbIdentityTPrec :: TextShow1 f
                       => (Int -> a -> Builder) -> ([a] -> Builder)
                       -> Int -> IdentityT f a -> Builder
liftShowbIdentityTPrec sp sl p (IdentityT m) =
    showbUnaryWith (liftShowbPrec sp sl) "IdentityT" p m
{-# INLINE liftShowbIdentityTPrec #-}

-- | Convert a 'ListT' value to a 'Builder' with the given show functions and precedence.
--
-- /Since: 3/
liftShowbListTPrec :: TextShow1 m
                   => (Int -> a -> Builder) -> ([a] -> Builder)
                   -> Int -> ListT m a -> Builder
liftShowbListTPrec sp sl p (ListT m) =
    showbUnaryWith (liftShowbPrec (liftShowbPrec sp sl)
                                  (liftShowbList sp sl)) "ListT" p m
{-# INLINE liftShowbListTPrec #-}

-- | Convert a 'MaybeT' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 3/
liftShowbMaybeTPrec :: TextShow1 m
                    => (Int -> a -> Builder) -> ([a] -> Builder)
                    -> Int -> MaybeT m a -> Builder
liftShowbMaybeTPrec sp sl p (MaybeT m) =
    showbUnaryWith (liftShowbPrec (liftShowbPrec sp sl)
                                  (liftShowbList sp sl)) "MaybeT" p m
{-# INLINE liftShowbMaybeTPrec #-}

-- | Convert a lazy 'WL.WriterT' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 3/
liftShowbWriterTLazyPrec :: (TextShow w, TextShow1 m)
                         => (Int -> a -> Builder) -> ([a] -> Builder)
                         -> Int -> WL.WriterT w m a -> Builder
liftShowbWriterTLazyPrec sp sl p (WL.WriterT m) =
    showbUnaryWith (liftShowbPrec (liftShowbPrec2 sp sl showbPrec showbList)
                                  (liftShowbList2 sp sl showbPrec showbList))
                   "WriterT" p m
{-# INLINE liftShowbWriterTLazyPrec #-}

-- | Convert a strict 'WS.WriterT' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 3/
liftShowbWriterTStrictPrec :: (TextShow w, TextShow1 m)
                           => (Int -> a -> Builder) -> ([a] -> Builder)
                           -> Int -> WS.WriterT w m a -> Builder
liftShowbWriterTStrictPrec sp sl p (WS.WriterT m) =
    showbUnaryWith (liftShowbPrec (liftShowbPrec2 sp sl showbPrec showbList)
                                  (liftShowbList2 sp sl showbPrec showbList))
                   "WriterT" p m
{-# INLINE liftShowbWriterTStrictPrec #-}

instance (TextShow e, TextShow1 m, TextShow a) => TextShow (ErrorT e m a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance (TextShow e, TextShow1 m) => TextShow1 (ErrorT e m) where
    liftShowbPrec = liftShowbErrorTPrec
    INLINE_INST_FUN(liftShowbPrec)

instance (TextShow e, TextShow1 m, TextShow a) => TextShow (ExceptT e m a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance (TextShow e, TextShow1 m) => TextShow1 (ExceptT e m) where
    liftShowbPrec = liftShowbExceptTPrec
    INLINE_INST_FUN(liftShowbPrec)

instance (TextShow1 f, TextShow a) => TextShow (IdentityT f a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 f => TextShow1 (IdentityT f) where
    liftShowbPrec = liftShowbIdentityTPrec
    INLINE_INST_FUN(liftShowbPrec)

instance (TextShow1 m, TextShow a) => TextShow (ListT m a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 m => TextShow1 (ListT m) where
    liftShowbPrec = liftShowbListTPrec
    INLINE_INST_FUN(liftShowbPrec)

instance (TextShow1 m, TextShow a) => TextShow (MaybeT m a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 m => TextShow1 (MaybeT m) where
    liftShowbPrec = liftShowbMaybeTPrec
    INLINE_INST_FUN(liftShowbPrec)

instance (TextShow w, TextShow1 m, TextShow a) => TextShow (WL.WriterT w m a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance (TextShow w, TextShow1 m) => TextShow1 (WL.WriterT w m) where
    liftShowbPrec = liftShowbWriterTLazyPrec
    INLINE_INST_FUN(liftShowbPrec)

instance (TextShow w, TextShow1 m, TextShow a) => TextShow (WS.WriterT w m a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance (TextShow w, TextShow1 m) => TextShow1 (WS.WriterT w m) where
    liftShowbPrec = liftShowbWriterTStrictPrec
    INLINE_INST_FUN(liftShowbPrec)
