{-|
Module:      Text.Show.Text.Instances
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Additional @Show@ instances not provided by @text-show@.

/Since: 0.1/
-}
module Text.Show.Text.Instances () where

import Text.Show.Text.Compiler.Hoopl            ()

import Text.Show.Text.Control.Applicative.Trans ()
import Text.Show.Text.Control.Monad.Trans       ()

import Text.Show.Text.Data.Bifunctor            ()
import Text.Show.Text.Data.Binary               ()
import Text.Show.Text.Data.Containers           ()
import Text.Show.Text.Data.Functor.Trans        ()
import Text.Show.Text.Data.List.NonEmpty        ()
import Text.Show.Text.Data.Semigroup            ()
import Text.Show.Text.Data.Tagged               ()
import Text.Show.Text.Data.Time                 ()
import Text.Show.Text.Data.UnorderedContainers  ()
import Text.Show.Text.Data.Vector               ()

import Text.Show.Text.Language.Haskell.TH       ()

import Text.Show.Text.System.Console.Haskeline  ()
import Text.Show.Text.System.Console.Terminfo   ()
import Text.Show.Text.System.Locale             ()
import Text.Show.Text.System.Posix              ()
import Text.Show.Text.System.Random             ()
import Text.Show.Text.System.Time               ()
import Text.Show.Text.System.Win32              ()

import Text.Show.Text.Text.PrettyPrint          ()

import Text.Show.Text.Trace.Hpc                 ()
