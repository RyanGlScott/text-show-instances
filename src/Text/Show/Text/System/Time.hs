{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.System.Time
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @old-time@ library.

/Since: 0.1/
-}
module Text.Show.Text.System.Time (
      showbClockTime
    , showbTimeDiffPrec
    , showbCalendarTimePrec
    , showbMonth
    , showbDay
    ) where

import Prelude hiding (Show)

import System.IO.Unsafe (unsafePerformIO)
import System.Time (ClockTime, TimeDiff, CalendarTime, Month, Day,
                    calendarTimeToString, toCalendarTime)

import Text.Show.Text (Show(showb, showbPrec), Builder, fromString)
import Text.Show.Text.TH (deriveShowPragmas, defaultInlineShowb, defaultInlineShowbPrec)

#include "inline.h"

-- | Convert a 'ClockTime' to a 'Builder'.
-- 
-- /Since: 0.1/
showbClockTime :: ClockTime -> Builder
showbClockTime = fromString . calendarTimeToString . unsafePerformIO . toCalendarTime
{-# INLINE showbClockTime #-}

-- | Convert a 'TimeDiff' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbTimeDiffPrec :: Int -> TimeDiff -> Builder
showbTimeDiffPrec = showbPrec
{-# INLINE showbTimeDiffPrec #-}

-- | Convert a 'CalendarTime' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbCalendarTimePrec :: Int -> CalendarTime -> Builder
showbCalendarTimePrec = showbPrec
{-# INLINE showbCalendarTimePrec #-}

-- | Convert a 'Month' to a 'Builder'.
-- 
-- /Since: 0.1/
showbMonth :: Month -> Builder
showbMonth = showb
{-# INLINE showbMonth #-}

-- | Convert a 'Day' to a 'Builder'.
-- 
-- /Since: 0.1/
showbDay :: Day -> Builder
showbDay = showb
{-# INLINE showbDay #-}

instance Show ClockTime where
    showb = showbClockTime
    INLINE_INST_FUN(showb)

$(deriveShowPragmas defaultInlineShowbPrec ''TimeDiff)
$(deriveShowPragmas defaultInlineShowbPrec ''CalendarTime)
$(deriveShowPragmas defaultInlineShowb     ''Month)
$(deriveShowPragmas defaultInlineShowb     ''Day)