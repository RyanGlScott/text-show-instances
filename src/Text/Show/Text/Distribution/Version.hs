{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Version
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Version@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Version (
      showbBound
    , showbLowerBoundPrec
    , showbUpperBoundPrec
    , showbVersionIntervalsPrec
    , showbVersionRangePrec
    ) where

import Distribution.Version (Bound, LowerBound, UpperBound,
                             VersionIntervals, VersionRange)

import Text.Show.Text (Builder, showb, showbPrec)
import Text.Show.Text.TH (deriveShow, deriveShowPragmas,
                          defaultInlineShowb, defaultInlineShowbPrec)

-- | Convert a 'Bound' to a 'Builder'.
-- 
-- /Since: 0.2/
showbBound :: Bound -> Builder
showbBound = showb
{-# INLINE showbBound #-}

-- | Convert a 'LowerBound' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbLowerBoundPrec :: Int -> LowerBound -> Builder
showbLowerBoundPrec = showbPrec
{-# INLINE showbLowerBoundPrec #-}

-- | Convert an 'UpperBound' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbUpperBoundPrec :: Int -> UpperBound -> Builder
showbUpperBoundPrec = showbPrec
{-# INLINE showbUpperBoundPrec #-}

-- | Convert a 'VersionIntervals' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbVersionIntervalsPrec :: Int -> VersionIntervals -> Builder
showbVersionIntervalsPrec = showbPrec
{-# INLINE showbVersionIntervalsPrec #-}

-- | Convert a 'VersionRange' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbVersionRangePrec :: Int -> VersionRange -> Builder
showbVersionRangePrec = showbPrec
{-# INLINE showbVersionRangePrec #-}

$(deriveShowPragmas defaultInlineShowb     ''Bound)
$(deriveShowPragmas defaultInlineShowbPrec ''LowerBound)
$(deriveShow                               ''UpperBound)
$(deriveShowPragmas defaultInlineShowbPrec ''VersionIntervals)
$(deriveShow                               ''VersionRange)