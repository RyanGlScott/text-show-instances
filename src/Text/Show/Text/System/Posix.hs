{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.System.Posix
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @unix@ library. This module is
only available on Unix-like operating systems (i.e., not Windows).

/Since: 0.1/
-}
module Text.Show.Text.System.Posix (
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
import Text.Show.Text.TH (deriveShowPragmas, defaultInlineShowb, defaultInlineShowbPrec)

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

$(deriveShowPragmas defaultInlineShowb     ''RTLDFlags)
$(deriveShowPragmas defaultInlineShowbPrec ''DL)
$(deriveShowPragmas defaultInlineShowbPrec ''ProcessStatus)
$(deriveShowPragmas defaultInlineShowbPrec ''GroupEntry)
$(deriveShowPragmas defaultInlineShowbPrec ''UserEntry)