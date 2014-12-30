{-|
Module:      Text.Show.Text.Data
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Imports 'Show' instances for @Data@ modules.
-}
module Text.Show.Text.Data () where

import Text.Show.Text.Data.Containers          ()
import Text.Show.Text.Data.Tagged              ()
import Text.Show.Text.Data.Time                ()
import Text.Show.Text.Data.UnorderedContainers ()
import Text.Show.Text.Data.Vector              ()