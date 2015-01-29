{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Simple.Compiler
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Simple.Compiler@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Simple.Compiler (
      showbCompilerPrec
    , showbDebugInfoLevel
    , showbOptimisationLevel
    , showbPackageDBPrec
    ) where

import Distribution.Simple.Compiler (Compiler, DebugInfoLevel,
                                     OptimisationLevel, PackageDB)

import Text.Show.Text (Builder, showb, showbPrec)
import Text.Show.Text.Data.Containers       ()
import Text.Show.Text.Distribution.Compiler ()
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'Compiler' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbCompilerPrec :: Int -> Compiler -> Builder
showbCompilerPrec = showbPrec
{-# INLINE showbCompilerPrec #-}

-- | Convert a 'DebugInfoLevel' to a 'Builder'.
-- 
-- /Since: 0.2/
showbDebugInfoLevel :: DebugInfoLevel -> Builder
showbDebugInfoLevel = showb
{-# INLINE showbDebugInfoLevel #-}

-- | Convert an 'OptimisationLevel' to a 'Builder'.
-- 
-- /Since: 0.2/
showbOptimisationLevel :: OptimisationLevel -> Builder
showbOptimisationLevel = showb
{-# INLINE showbOptimisationLevel #-}

-- | Convert a 'PackageDB' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbPackageDBPrec :: Int -> PackageDB -> Builder
showbPackageDBPrec = showbPrec
{-# INLINE showbPackageDBPrec #-}

$(deriveShow ''Compiler)
$(deriveShow ''DebugInfoLevel)
$(deriveShow ''OptimisationLevel)
$(deriveShow ''PackageDB)