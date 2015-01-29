{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Verbosity
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Verbosity' values.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Verbosity (showbVerbosity) where

import Distribution.Verbosity (Verbosity)

import Text.Show.Text (Builder, showb)
import Text.Show.Text.TH (deriveShowPragmas, defaultInlineShowb)

-- | Convert a 'Verbosity' value to a 'Builder'.
-- 
-- /Since: 0.2/
showbVerbosity :: Verbosity -> Builder
showbVerbosity = showb
{-# INLINE showbVerbosity #-}

$(deriveShowPragmas defaultInlineShowb ''Verbosity)