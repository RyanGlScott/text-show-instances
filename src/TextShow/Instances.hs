{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 702
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
#endif

{-|
Module:      TextShow.Instances
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Additional 'TextShow', 'TextShow1', and 'TextShow2' instances not provided
by @text-show@.

/Since: 2/
-}
module TextShow.Instances (
    -- * Class re-exports
      TextShow(..)
    , TextShow1(..)
    , TextShow2(..)
    -- * Module re-export
    , module TextShow
    ) where

import TextShow

import TextShow.Compiler.Hoopl            ()

import TextShow.Control.Applicative.Trans ()
import TextShow.Control.Monad.Trans       ()

import TextShow.Data.Bifunctor            ()
import TextShow.Data.Binary               ()
import TextShow.Data.Containers           ()
import TextShow.Data.Functor.Trans        ()
import TextShow.Data.List.NonEmpty        ()
import TextShow.Data.Semigroup            ()
import TextShow.Data.Tagged               ()
import TextShow.Data.Time                 ()
import TextShow.Data.UnorderedContainers  ()
import TextShow.Data.Vector               ()

import TextShow.Language.Haskell.TH       ()

import TextShow.System.Console.Haskeline  ()
import TextShow.System.Console.Terminfo   ()
import TextShow.System.Locale             ()
import TextShow.System.Posix              ()
import TextShow.System.Random             ()
import TextShow.System.Time               ()
import TextShow.System.Win32              ()

import TextShow.Text.PrettyPrint          ()

import TextShow.Trace.Hpc                 ()
