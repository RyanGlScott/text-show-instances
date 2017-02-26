{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.System.Console.Haskeline
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @haskeline@ library.

/Since: 2/
-}
module TextShow.System.Console.Haskeline (
      showbInterrupt
    , showbPrefsPrec
    , showbCompletionPrec
    , showbHistory
    ) where

import System.Console.Haskeline (Interrupt, Prefs)
import System.Console.Haskeline.Completion (Completion)
import System.Console.Haskeline.History (History, historyLines)

import TextShow (TextShow(..), Builder, FromStringShow(..))
import TextShow.TH (deriveTextShow)
import TextShow.Utils (showbUnaryListWith)

-- | Convert an 'Interrupt' to a 'Builder'.
--
-- /Since: 2/
showbInterrupt :: Interrupt -> Builder
showbInterrupt = showb
{-# INLINE showbInterrupt #-}

-- | Convert a 'Prefs' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbPrefsPrec :: Int -> Prefs -> Builder
showbPrefsPrec p = showbPrec p . FromStringShow
{-# INLINE showbPrefsPrec #-}

-- | Convert a 'Completion' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCompletionPrec :: Int -> Completion -> Builder
showbCompletionPrec = showbPrec
{-# INLINE showbCompletionPrec #-}

-- | Convert a 'History' value to a 'Builder'.
--
-- /Since: 2/
showbHistory :: History -> Builder
showbHistory = showbUnaryListWith showb 0 . historyLines
{-# INLINE showbHistory #-}

$(deriveTextShow ''Interrupt)

instance TextShow Prefs where
    showbPrec = showbPrefsPrec
    {-# INLINE showbPrec #-}

$(deriveTextShow ''Completion)

instance TextShow History where
    showb = showbHistory
    {-# INLINE showb #-}
