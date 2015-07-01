{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.System.Console.Haskeline
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @haskeline@ library.

/Since: 0.2/
-}
module Text.Show.Text.System.Console.Haskeline (
      showbInterrupt
    , showbPrefsPrec
    , showbCompletionPrec
    , showbHistory
    ) where

import Prelude hiding (Show)

import System.Console.Haskeline (Interrupt, Prefs)
import System.Console.Haskeline.Completion (Completion)
import System.Console.Haskeline.History (History, historyLines)

import Text.Show.Text (Show(showb, showbPrec), Builder, FromStringShow(..))
import Text.Show.Text.TH (deriveShow)
import Text.Show.Text.Utils (showbUnaryListWith)

#include "inline.h"

-- | Convert an 'Interrupt' to a 'Builder'.
--
-- /Since: 0.2/
showbInterrupt :: Interrupt -> Builder
showbInterrupt = showb
{-# INLINE showbInterrupt #-}

-- | Convert a 'Prefs' value to a 'Builder' with the given precedence.
--
-- /Since: 0.2/
showbPrefsPrec :: Int -> Prefs -> Builder
showbPrefsPrec p = showbPrec p . FromStringShow
{-# INLINE showbPrefsPrec #-}

-- | Convert a 'Completion' value to a 'Builder' with the given precedence.
--
-- /Since: 0.2/
showbCompletionPrec :: Int -> Completion -> Builder
showbCompletionPrec = showbPrec
{-# INLINE showbCompletionPrec #-}

-- | Convert a 'History' value to a 'Builder'.
--
-- /Since: 0.2/
showbHistory :: History -> Builder
showbHistory = showbUnaryListWith showb 0 . historyLines
{-# INLINE showbHistory #-}

$(deriveShow ''Interrupt)

instance Show Prefs where
    showbPrec = showbPrefsPrec
    INLINE_INST_FUN(showbPrec)

$(deriveShow ''Completion)

instance Show History where
    showb = showbHistory
    INLINE_INST_FUN(showb)
