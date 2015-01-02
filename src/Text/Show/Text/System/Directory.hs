{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.System.Directory
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Permissions'.

/Since: 0.1/
-}
module Text.Show.Text.System.Directory (showbPermissionsPrec) where

import Prelude hiding (Show)

import System.Directory (Permissions)

import Text.Show.Text (Show(showbPrec), Builder)
import Text.Show.Text.TH (deriveShowPragmas, defaultInlineShowbPrec)

-- | Convert 'Permissions' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbPermissionsPrec :: Int -> Permissions -> Builder
showbPermissionsPrec = showbPrec
{-# INLINE showbPermissionsPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''Permissions)