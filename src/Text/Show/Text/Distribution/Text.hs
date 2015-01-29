{-|
Module:      Text.Show.Text.Distribution.Text
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Exports 'displayB', a 'Builder' equivalent of the 'display' function in the
@Distribution.Text@ module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Text (displayB) where

import Distribution.Text (Text(disp))

import Text.PrettyPrint.HughesPJ (Style(..), Mode(PageMode))
import Text.Show.Text (Builder)
import Text.Show.Text.Text.PrettyPrint (renderStyleB)

-- | Render a 'Text' instance to a 'Builder'.
-- 
-- /Since: 0.2/
displayB :: Text a => a -> Builder
displayB = renderStyleB style . disp
    where style = Style {
        mode           = PageMode
      , lineLength     = 79
      , ribbonsPerLine = 1.0
    }