{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.System.Directory
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Permissions'.

/Since: 2/
-}
module TextShow.System.Directory (showbPermissionsPrec) where

import System.Directory (Permissions)

import TextShow (TextShow(..), Builder)
import TextShow.TH (deriveTextShow)

-- | Convert 'Permissions' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbPermissionsPrec :: Int -> Permissions -> Builder
showbPermissionsPrec = showbPrec
{-# INLINE showbPermissionsPrec #-}

$(deriveTextShow ''Permissions)
