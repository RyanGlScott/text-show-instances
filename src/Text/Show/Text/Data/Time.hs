{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
#if MIN_VERSION_time(1,5,0)
{-# LANGUAGE TemplateHaskell   #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Time
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @time@ library.

/Since: 0.1/
-}
module Text.Show.Text.Data.Time (
      showbDay
    , showbDiffTime
    , showbUTCTime
    , showbNominalDiffTime
    , showbAbsoluteTime
    , showbTimeZone
    , showbTimeOfDay
    , showbLocalTime
    , showbZonedTime
#if MIN_VERSION_time(1,5,0)
    , showbTimeLocalePrec
#endif
    ) where

import Data.Fixed (Pico)
import Data.Monoid.Compat
import Data.Semigroup (timesN)
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Clock (DiffTime, UTCTime, NominalDiffTime)
import Data.Time.Clock.TAI (AbsoluteTime, taiToUTCTime)
import Data.Time.Format (NumericPadOption)
import Data.Time.LocalTime (TimeZone(..), TimeOfDay(..), LocalTime(..), ZonedTime(..),
                            utc, utcToLocalTime, utcToZonedTime)

import Prelude hiding (Show)

import Text.Show.Text (Show(showb), Builder, FromStringShow(..),
                       fromString, lengthB, showbSpace, singleton)
import Text.Show.Text.Data.Fixed (showbFixed)
import Text.Show.Text.Data.Integral ()

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (TimeLocale)

import Text.Show.Text (showbPrec)
import Text.Show.Text.TH (deriveShow)
#endif

#include "inline.h"

-- | Convert a 'Day' into a 'Builder'.
--
-- /Since: 0.1/
showbDay :: Day -> Builder
showbDay = showbGregorian
{-# INLINE showbDay #-}

-- | Convert a 'DiffTime' into a 'Builder'.
--
-- /Since: 0.1/
showbDiffTime :: DiffTime -> Builder
showbDiffTime = showb . FromStringShow
{-# INLINE showbDiffTime #-}

-- | Convert a 'UTCTime' into a 'Builder'.
--
-- /Since: 0.1/
showbUTCTime :: UTCTime -> Builder
showbUTCTime = showb . utcToZonedTime utc
{-# INLINE showbUTCTime #-}

-- | Convert a 'NominalDiffTime' into a 'Builder'.
--
-- /Since: 0.1/
showbNominalDiffTime :: NominalDiffTime -> Builder
showbNominalDiffTime = showb . FromStringShow
{-# INLINE showbNominalDiffTime #-}

-- | Convert a 'AbsoluteTime' into a 'Builder'.
--
-- /Since: 0.1/
showbAbsoluteTime :: AbsoluteTime -> Builder
showbAbsoluteTime t = showbLocalTime (utcToLocalTime utc $ taiToUTCTime (const 0) t)
                      <> " TAI" -- ugly, but standard apparently
{-# INLINE showbAbsoluteTime #-}

-- | Convert a 'TimeZone' into a 'Builder'.
--
-- /Since: 0.1/
showbTimeZone :: TimeZone -> Builder
showbTimeZone zone@(TimeZone _ _ "") = timeZoneOffsetBuilder zone
showbTimeZone (TimeZone _ _ name)    = fromString name
{-# INLINE showbTimeZone #-}

-- | Convert a 'TimeOfDay' into a 'Builder'.
--
-- /Since: 0.1/
showbTimeOfDay :: TimeOfDay -> Builder
showbTimeOfDay (TimeOfDay h m sec) = showb2      zeroOpt h
                                  <> singleton ':'
                                  <> showb2      zeroOpt m
                                  <> singleton ':'
                                  <> showb2Fixed zeroOpt sec
{-# INLINE showbTimeOfDay #-}

-- | Convert a 'LocalTime' into a 'Builder'.
--
-- /Since: 0.1/
showbLocalTime :: LocalTime -> Builder
showbLocalTime (LocalTime d t) = showbGregorian d <> showbSpace <> showb t
{-# INLINE showbLocalTime #-}

-- | Convert a 'ZonedTime' into a 'Builder'.
--
-- /Since: 0.1/
showbZonedTime :: ZonedTime -> Builder
showbZonedTime (ZonedTime t zone) = showb t <> showbSpace <> showb zone
{-# INLINE showbZonedTime #-}

pad1 :: NumericPadOption -> Builder -> Builder
pad1 (Just c) b = singleton c <> b
pad1 _        b = b
{-# INLINE pad1 #-}

padN :: Int -> Char -> Builder -> Builder
padN i _ b | i <= 0 = b
padN i c b          = timesN (fromIntegral i) (singleton c) <> b
{-# INLINE padN #-}

showb2 :: (Num t, Ord t, Show t) => NumericPadOption -> t -> Builder
showb2 = showbPaddedMin 2
{-# INLINE showb2 #-}

showb2Fixed :: NumericPadOption -> Pico -> Builder
showb2Fixed opt x | x < 10 = pad1 opt $ showbFixed True x
showb2Fixed _   x          = showbFixed True x
{-# INLINE showb2Fixed #-}

showb4 :: (Num t, Ord t, Show t) => NumericPadOption -> t -> Builder
showb4 = showbPaddedMin 4
{-# INLINE showb4 #-}

showbGregorian :: Day -> Builder
showbGregorian date = showb4 zeroOpt y
                   <> singleton '-'
                   <> showb2 zeroOpt m
                   <> singleton '-'
                   <> showb2 zeroOpt d
  where
    (y,m,d) = toGregorian date

showbPaddedMin :: (Num t, Ord t, Show t) => Int -> NumericPadOption -> t -> Builder
showbPaddedMin _  Nothing  i = showb i
showbPaddedMin pl opt      i | i < 0 = singleton '-' <> showbPaddedMin pl opt (negate i)
showbPaddedMin pl (Just c) i =
    let b = showb i
    in padN (pl - fromIntegral (lengthB b)) c b

showbT :: NumericPadOption -> Int -> Builder
showbT opt t = showb4 opt ((div t 60) * 100 + (mod t 60))
{-# INLINE showbT #-}

timeZoneOffsetBuilder' :: NumericPadOption -> TimeZone -> Builder
timeZoneOffsetBuilder' opt (TimeZone t _ _) | t < 0 = singleton '-' <> showbT opt (negate t)
timeZoneOffsetBuilder' opt (TimeZone t _ _) = singleton '+' <> showbT opt t
{-# INLINE timeZoneOffsetBuilder' #-}

timeZoneOffsetBuilder :: TimeZone -> Builder
timeZoneOffsetBuilder = timeZoneOffsetBuilder' $ Just '0'
{-# INLINE timeZoneOffsetBuilder #-}

zeroOpt :: NumericPadOption
zeroOpt = Just '0'
{-# INLINE zeroOpt #-}

#if MIN_VERSION_time(1,5,0)
-- | Convert a 'TimeLocale' to a 'Builder' with the given precedence. This function is
-- available with @time-1.5@ or later.
--
-- /Since: 0.2/
showbTimeLocalePrec :: Int -> TimeLocale -> Builder
showbTimeLocalePrec = showbPrec
{-# INLINE showbTimeLocalePrec #-}
#endif

instance Show Day where
    showb = showbDay
    INLINE_INST_FUN(showb)

instance Show DiffTime where
    showb = showbDiffTime
    INLINE_INST_FUN(showb)

instance Show UTCTime where
    showb = showbUTCTime
    INLINE_INST_FUN(showb)

instance Show NominalDiffTime where
    showb = showbNominalDiffTime
    INLINE_INST_FUN(showb)

instance Show AbsoluteTime where
    showb = showbAbsoluteTime
    INLINE_INST_FUN(showb)

instance Show TimeZone where
    showb = showbTimeZone
    INLINE_INST_FUN(showb)

instance Show TimeOfDay where
    showb = showbTimeOfDay
    INLINE_INST_FUN(showb)

instance Show LocalTime where
    showb = showbLocalTime
    INLINE_INST_FUN(showb)

instance Show ZonedTime where
    showb = showbZonedTime
    INLINE_INST_FUN(showb)

#if MIN_VERSION_time(1,5,0)
$(deriveShow ''TimeLocale)
#endif
