{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Simple.BuildTarget
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Simple.BuildTarget@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Simple.BuildTarget (
      showbBuildTargetPrec
    , showbBuildTargetProblemPrec
    , showbUserBuildTargetPrec
    , showbUserBuildTargetProblemPrec
    ) where

import Distribution.Simple.BuildTarget (BuildTarget, BuildTargetProblem,
                                        UserBuildTarget, UserBuildTargetProblem)

import Text.Show.Text (Builder, showbPrec)
import Text.Show.Text.Distribution.ModuleName            ()
import Text.Show.Text.Distribution.Simple.LocalBuildInfo ()
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'BuildTarget' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbBuildTargetPrec :: Int -> BuildTarget -> Builder
showbBuildTargetPrec = showbPrec
{-# INLINE showbBuildTargetPrec #-}

-- | Convert a 'BuildTargetProblem' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbBuildTargetProblemPrec :: Int -> BuildTargetProblem -> Builder
showbBuildTargetProblemPrec = showbPrec
{-# INLINE showbBuildTargetProblemPrec #-}

-- | Convert a 'UserBuildTarget' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbUserBuildTargetPrec :: Int -> UserBuildTarget -> Builder
showbUserBuildTargetPrec = showbPrec
{-# INLINE showbUserBuildTargetPrec #-}

-- | Convert a 'UserBuildTargetProblem' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbUserBuildTargetProblemPrec :: Int -> UserBuildTargetProblem -> Builder
showbUserBuildTargetProblemPrec = showbPrec
{-# INLINE showbUserBuildTargetProblemPrec #-}

$(deriveShow ''BuildTarget)
$(deriveShow ''BuildTargetProblem)
$(deriveShow ''UserBuildTarget)
$(deriveShow ''UserBuildTargetProblem)