{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
#if MIN_VERSION_time(1,5,0)
{-# LANGUAGE TemplateHaskell   #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Time
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @time@ library.

/Since: 2/
-}
module TextShow.Data.Time () where

import Data.Fixed (Pico)
import Data.Monoid.Compat
import Data.Semigroup (mtimesDefault)
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Clock (DiffTime, UTCTime, NominalDiffTime, UniversalTime)
import Data.Time.Clock.TAI (AbsoluteTime, taiToUTCTime)
import Data.Time.Format (NumericPadOption)
import Data.Time.LocalTime (TimeZone(..), TimeOfDay(..), LocalTime(..), ZonedTime(..),
                            ut1ToLocalTime, utc, utcToLocalTime, utcToZonedTime)

import TextShow (TextShow(..), Builder, FromStringShow(..),
                 fromString, lengthB, showbSpace, singleton)
import TextShow.Data.Fixed (showbFixed)
import TextShow.Data.Integral ()

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (TimeLocale)
import TextShow.TH (deriveTextShow)
#endif

#if MIN_VERSION_time(1,7,0)
import Data.Maybe (fromJust)
#endif

#if MIN_VERSION_time(1,8,0)
import Data.Time.Clock.System (SystemTime)
#endif

pad1 :: NumericPadOption -> Builder -> Builder
pad1 (Just c) b = singleton c <> b
pad1 _        b = b
{-# INLINE pad1 #-}

padN :: Int -> Char -> Builder -> Builder
padN i _ b | i <= 0 = b
padN i c b          = mtimesDefault i (singleton c) <> b
{-# INLINE padN #-}

showb2 :: (Num t, Ord t, TextShow t) => NumericPadOption -> t -> Builder
showb2 = showbPaddedMin 2
{-# INLINE showb2 #-}

showb2Fixed :: NumericPadOption -> Pico -> Builder
showb2Fixed opt x | x < 10 = pad1 opt $ showbFixed True x
showb2Fixed _   x          = showbFixed True x
{-# INLINE showb2Fixed #-}

showb4 :: (Num t, Ord t, TextShow t) => NumericPadOption -> t -> Builder
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

showbPaddedMin :: (Num t, Ord t, TextShow t) => Int -> NumericPadOption -> t -> Builder
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

-- | /Since: 2/
instance TextShow Day where
    showb = showbGregorian
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow DiffTime where
    showb = showb . FromStringShow
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow UTCTime where
    showb = showb . utcToZonedTime utc
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow NominalDiffTime where
    showb = showb . FromStringShow
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow AbsoluteTime where
    showb t = showb (utcToLocalTime utc $
#if MIN_VERSION_time(1,7,0)
                                          fromJust $ taiToUTCTime (const (Just 0)) t)
#else
                                          taiToUTCTime (const 0) t)
#endif
              <> " TAI" -- ugly, but standard apparently
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow TimeZone where
    showb zone@(TimeZone _ _ "") = timeZoneOffsetBuilder zone
    showb (TimeZone _ _ name)    = fromString name
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow TimeOfDay where
    showb (TimeOfDay h m sec) = showb2      zeroOpt h
                             <> singleton ':'
                             <> showb2      zeroOpt m
                             <> singleton ':'
                             <> showb2Fixed zeroOpt sec
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow LocalTime where
    showb (LocalTime d t) = showbGregorian d <> showbSpace <> showb t
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow ZonedTime where
    showb (ZonedTime t zone) = showb t <> showbSpace <> showb zone
    {-# INLINE showb #-}

-- | /Since: 3.6/
instance TextShow UniversalTime where
    showb t = showb $ ut1ToLocalTime 0 t
    {-# INLINE showb #-}

#if MIN_VERSION_time(1,5,0)
-- | Only available with @time-1.5@ or later.
--
-- /Since: 2/
$(deriveTextShow ''TimeLocale)
#endif

#if MIN_VERSION_time(1,8,0)
-- | Only available with @time-1.8@ or later.
--
-- /Since: 3.6/
$(deriveTextShow ''SystemTime)
#endif
