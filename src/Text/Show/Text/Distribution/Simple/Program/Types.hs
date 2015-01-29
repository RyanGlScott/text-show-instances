{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Simple.Program.Types
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Simple.Program.Types@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Simple.Program.Types (
      showbConfiguredProgramPrec
    , showbProgramLocationPrec
    ) where

import Distribution.Simple.Program.Types (ConfiguredProgram, ProgramLocation)

import Text.Show.Text (Builder, showbPrec)
import Text.Show.Text.Data.Containers ()
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'ConfiguredProgram' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbConfiguredProgramPrec :: Int -> ConfiguredProgram -> Builder
showbConfiguredProgramPrec = showbPrec
{-# INLINE showbConfiguredProgramPrec #-}

-- | Convert a 'ProgramLocation' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbProgramLocationPrec :: Int -> ProgramLocation -> Builder
showbProgramLocationPrec = showbPrec
{-# INLINE showbProgramLocationPrec #-}

$(deriveShow ''ConfiguredProgram)
$(deriveShow ''ProgramLocation)