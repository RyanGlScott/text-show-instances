{-# LANGUAGE CPP             #-}

#if !defined(mingw32_HOST_OS)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Text.Show.Text.System.Posix
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' functions for data types in the @unix@ library. This module
only exports functions if using a Unix-like operating system (i.e., not Windows).

/Since: 0.1/
-}
module Text.Show.Text.System.Posix (
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

import Text.Show.Text (Builder, showb, showbPrec)
import Text.Show.Text.TH (deriveShow)

-- | Convert an 'RTLDFlags' value to a 'Builder'.
--
-- /Since: 0.1/
showbRTLDFlags :: RTLDFlags -> Builder
showbRTLDFlags = showb
{-# INLINE showbRTLDFlags #-}

-- | Convert a 'DL' value to a 'Builder' with the given precedence.
--
-- /Since: 0.1/
showbDLPrec :: Int -> DL -> Builder
showbDLPrec = showbPrec
{-# INLINE showbDLPrec #-}

-- | Convert a 'ProcessStatus' to a 'Builder' with the given precedence.
--
-- /Since: 0.1/
showbProcessStatusPrec :: Int -> ProcessStatus -> Builder
showbProcessStatusPrec = showbPrec
{-# INLINE showbProcessStatusPrec #-}

-- | Convert a 'GroupEntry' to a 'Builder' with the given precedence.
--
-- /Since: 0.1/
showbGroupEntryPrec :: Int -> GroupEntry -> Builder
showbGroupEntryPrec = showbPrec
{-# INLINE showbGroupEntryPrec #-}

-- | Convert a 'UserEntry' to a 'Builder' with the given precedence.
--
-- /Since: 0.1/
showbUserEntryPrec :: Int -> UserEntry -> Builder
showbUserEntryPrec = showbPrec
{-# INLINE showbUserEntryPrec #-}

$(deriveShow ''RTLDFlags)
$(deriveShow ''DL)
$(deriveShow ''ProcessStatus)
$(deriveShow ''GroupEntry)
$(deriveShow ''UserEntry)
#endif
