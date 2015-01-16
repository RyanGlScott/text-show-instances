{-# LANGUAGE CPP #-}
{-|
Module:      Text.Show.Text.System
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Imports 'Show' instances for @System@ modules.
-}
module Text.Show.Text.System () where

import Text.Show.Text.System.Console.Haskeline ()
import Text.Show.Text.System.Locale            ()
import Text.Show.Text.System.Random            ()
import Text.Show.Text.System.Time              ()

#if defined(mingw32_HOST_OS)
import Text.Show.Text.System.Win32             ()
#else
import Text.Show.Text.System.Console.Terminfo  ()
import Text.Show.Text.System.Posix             ()
#endif