{-# LANGUAGE CPP             #-}

#if !defined(mingw32_HOST_OS)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      TextShow.System.Console.Terminfo
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @terminfo@ library. This module
only exports functions if using a Unix-like operating system (i.e., not Windows).

/Since: 2/
-}
module TextShow.System.Console.Terminfo (
#if defined(mingw32_HOST_OS)
    ) where
#else
      showbColorPrec
    , showbSetupTermError
    ) where

import System.Console.Terminfo.Base (SetupTermError)
import System.Console.Terminfo.Color (Color)

import TextShow (TextShow(..), Builder, FromStringShow(..))
import TextShow.TH (deriveTextShow)

#include "inline.h"

-- | Convert a 'Color' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbColorPrec :: Int -> Color -> Builder
showbColorPrec = showbPrec
{-# INLINE showbColorPrec #-}

-- | Convert a 'SetupTermError' to a 'Builder'.
--
-- /Since: 2/
showbSetupTermError :: SetupTermError -> Builder
showbSetupTermError = showb . FromStringShow
{-# INLINE showbSetupTermError #-}

$(deriveTextShow ''Color)

instance TextShow SetupTermError where
    showb = showbSetupTermError
    INLINE_INST_FUN(showb)
#endif
