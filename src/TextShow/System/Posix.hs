{-# LANGUAGE CPP             #-}

#if !defined(mingw32_HOST_OS)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      TextShow.System.Posix
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @unix@ library. This module
only exports functions if using a Unix-like operating system (i.e., not Windows).

/Since: 2/
-}
module TextShow.System.Posix (
#if defined(mingw32_HOST_OS)
    ) where
#else
      showbRTLDFlags
    , showbDLPrec
    , showbProcessStatusPrec
    , showbGroupEntryPrec
    , showbUserEntryPrec
    ) where

import System.Posix.DynamicLinker (RTLDFlags, DL)
import System.Posix.Process (ProcessStatus)
import System.Posix.User (GroupEntry, UserEntry)

import TextShow (Builder, showb, showbPrec)
import TextShow.TH (deriveTextShow)

-- | Convert an 'RTLDFlags' value to a 'Builder'.
--
-- /Since: 2/
showbRTLDFlags :: RTLDFlags -> Builder
showbRTLDFlags = showb
{-# INLINE showbRTLDFlags #-}

-- | Convert a 'DL' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbDLPrec :: Int -> DL -> Builder
showbDLPrec = showbPrec
{-# INLINE showbDLPrec #-}

-- | Convert a 'ProcessStatus' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbProcessStatusPrec :: Int -> ProcessStatus -> Builder
showbProcessStatusPrec = showbPrec
{-# INLINE showbProcessStatusPrec #-}

-- | Convert a 'GroupEntry' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbGroupEntryPrec :: Int -> GroupEntry -> Builder
showbGroupEntryPrec = showbPrec
{-# INLINE showbGroupEntryPrec #-}

-- | Convert a 'UserEntry' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbUserEntryPrec :: Int -> UserEntry -> Builder
showbUserEntryPrec = showbPrec
{-# INLINE showbUserEntryPrec #-}

$(deriveTextShow ''RTLDFlags)
$(deriveTextShow ''DL)
$(deriveTextShow ''ProcessStatus)
$(deriveTextShow ''GroupEntry)
$(deriveTextShow ''UserEntry)
#endif
