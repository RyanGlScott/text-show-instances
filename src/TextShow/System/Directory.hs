{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.System.Directory
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Permissions'.

/Since: 2/
-}
module TextShow.System.Directory () where

import System.Directory (Permissions)
#if MIN_VERSION_directory(1,2,3)
import System.Directory (XdgDirectory)
#endif

import TextShow.TH (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''Permissions)

#if MIN_VERSION_directory(1,2,3)
-- | Only available with @directory-1.2.3.0@ or later.
--
-- /Since: 3.6/
$(deriveTextShow ''XdgDirectory)
#endif
