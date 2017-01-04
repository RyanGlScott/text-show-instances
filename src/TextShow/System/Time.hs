{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.System.Time
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @old-time@ library.

/Since: 2/
-}
module TextShow.System.Time (
      showbClockTime
    , showbTimeDiffPrec
    , showbCalendarTimePrec
    , showbMonth
    , showbDay
    ) where

import System.IO.Unsafe (unsafePerformIO)
import System.Time (ClockTime, TimeDiff, CalendarTime, Month, Day,
                    calendarTimeToString, toCalendarTime)

import TextShow (TextShow(..), Builder, fromString)
import TextShow.TH (deriveTextShow)

#include "inline.h"

-- | Convert a 'ClockTime' to a 'Builder'.
--
-- /Since: 2/
showbClockTime :: ClockTime -> Builder
showbClockTime = fromString . calendarTimeToString . unsafePerformIO . toCalendarTime

-- | Convert a 'TimeDiff' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbTimeDiffPrec :: Int -> TimeDiff -> Builder
showbTimeDiffPrec = showbPrec
{-# INLINE showbTimeDiffPrec #-}

-- | Convert a 'CalendarTime' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCalendarTimePrec :: Int -> CalendarTime -> Builder
showbCalendarTimePrec = showbPrec
{-# INLINE showbCalendarTimePrec #-}

-- | Convert a 'Month' to a 'Builder'.
--
-- /Since: 2/
showbMonth :: Month -> Builder
showbMonth = showb
{-# INLINE showbMonth #-}

-- | Convert a 'Day' to a 'Builder'.
--
-- /Since: 2/
showbDay :: Day -> Builder
showbDay = showb
{-# INLINE showbDay #-}

instance TextShow ClockTime where
    showb = showbClockTime
    INLINE_INST_FUN(showb)

$(deriveTextShow ''TimeDiff)
$(deriveTextShow ''CalendarTime)
$(deriveTextShow ''Month)
$(deriveTextShow ''Day)
