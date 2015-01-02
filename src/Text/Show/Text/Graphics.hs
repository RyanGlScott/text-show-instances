{-# LANGUAGE CPP #-}
{-|
Module:      Text.Show.Text.Graphics
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Imports 'Show' instances for @Graphics@ modules.
-}
module Text.Show.Text.Graphics () where

#if defined(mingw32_HOST_OS)
import Text.Show.Text.Graphics.Win32 ()
#endif