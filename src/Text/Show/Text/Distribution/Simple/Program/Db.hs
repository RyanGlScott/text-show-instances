{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Simple.Program.Db
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Simple.Program.Db@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Simple.Program.Db (showbProgramDb) where

import Distribution.Simple.Program.Db (ProgramDb)
import Prelude hiding (Show)
import Text.Show.Text (Show(showb), Builder, FromStringShow(..))

-- | Convert a 'ProgramDb' to a 'Builder'.
-- 
-- /Since: 0.2/
showbProgramDb :: ProgramDb -> Builder
showbProgramDb = showb . FromStringShow
{-# INLINE showbProgramDb #-}

instance Show ProgramDb where
    showb = showbProgramDb