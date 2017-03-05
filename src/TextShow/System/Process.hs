{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.System.Process
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @process@ library.
Only provided if using @process-1.4.3.0@ or later.

/Since: next/
-}
module TextShow.System.Process () where

#if MIN_VERSION_process(1,4,3)
import System.Process (CmdSpec, CreateProcess, StdStream)
import TextShow.TH (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''CmdSpec)
-- | /Since: next/
$(deriveTextShow ''CreateProcess)
-- | /Since: next/
$(deriveTextShow ''StdStream)
#endif
