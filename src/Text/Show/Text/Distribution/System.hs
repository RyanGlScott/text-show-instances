{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Compiler
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.System@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.System (
      showbArchPrec
    , showbOSPrec
    , showbPlatformPrec
    ) where

import Distribution.System (Arch, OS, Platform)

import Text.Show.Text (Builder, showbPrec)
import Text.Show.Text.TH (deriveShow, deriveShowPragmas, defaultInlineShowbPrec)

-- | Convert an 'Arch' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbArchPrec :: Int -> Arch -> Builder
showbArchPrec = showbPrec
{-# INLINE showbArchPrec #-}

-- | Convert an 'OS' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbOSPrec :: Int -> OS -> Builder
showbOSPrec = showbPrec
{-# INLINE showbOSPrec #-}

-- | Convert a 'Platform' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbPlatformPrec :: Int -> Platform -> Builder
showbPlatformPrec = showbPrec
{-# INLINE showbPlatformPrec #-}

$(deriveShow                               ''Arch)
$(deriveShow                               ''OS)
$(deriveShowPragmas defaultInlineShowbPrec ''Platform)