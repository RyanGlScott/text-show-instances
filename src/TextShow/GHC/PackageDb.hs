{-# LANGUAGE CPP             #-}

#if defined(MIN_VERSION_ghc_boot)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      TextShow.GHC.PackageDb
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the "GHC.PackageDb" module.
This module only exports functions if using @ghc-boot@.

/Since: 3.3/
-}
module TextShow.GHC.PackageDb (
#if !defined(MIN_VERSION_ghc_boot)
    ) where
#else
      liftShowbInstalledPackageInfoPrec2
# if __GLASGOW_HASKELL__ >= 801
    , liftShowbDbModulePrec2
# else
    , liftShowbOriginalModulePrec2
    , liftShowbExposedModulePrec2
# endif
    ) where

import GHC.PackageDb

import TextShow (TextShow(..), TextShow2(..), Builder)
import TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)

-- | Convert an 'InstalledPackageInfo' value to a 'Builder' with the given
-- show functions and precedence.
-- This function is only available when using @ghc-boot@ and GHC 8.1 or later.
--
-- /Since: 3.3/
liftShowbInstalledPackageInfoPrec2 :: ( TextShow srcpkgid
                                      , TextShow srcpkgname
# if __GLASGOW_HASKELL__ >= 801
                                      , TextShow unitid
# endif
                                      )
                                   => (Int -> a -> Builder) -> ([a] -> Builder)
                                   -> (Int -> b -> Builder) -> ([b] -> Builder)
                                   -> Int
                                   -> InstalledPackageInfo srcpkgid srcpkgname
# if __GLASGOW_HASKELL__ >= 801
                                                           unitid
# endif
                                                           a b
                                   -> Builder
liftShowbInstalledPackageInfoPrec2 = liftShowbPrec2

# if __GLASGOW_HASKELL__ >= 801
-- | Convert a 'DbModule' value to a 'Builder' with the given show functions
-- and precedence.
-- This function is only available when using @ghc-boot@.
--
-- /Since: 3.3/
liftShowbDbModulePrec2 :: (Int -> unitid     -> Builder) -> ([unitid]     -> Builder)
                       -> (Int -> modulename -> Builder) -> ([modulename] -> Builder)
                       -> Int -> DbModule unitid modulename -> Builder
liftShowbDbModulePrec2 = liftShowbPrec2
# else
-- | Convert an 'OriginalModule' value to a 'Builder' with the given show functions
-- and precedence.
-- This function is only available when using @ghc-boot@.
--
-- /Since: 3.3/
liftShowbOriginalModulePrec2 :: (Int -> unitid     -> Builder) -> ([unitid]     -> Builder)
                             -> (Int -> modulename -> Builder) -> ([modulename] -> Builder)
                             -> Int -> OriginalModule unitid modulename -> Builder
liftShowbOriginalModulePrec2 = liftShowbPrec2

-- | Convert an 'ExposedModule' value to a 'Builder' with the given show functions
-- and precedence.
-- This function is only available when using @ghc-boot@.
--
-- /Since: 3.3/
liftShowbExposedModulePrec2 :: (Int -> unitid     -> Builder) -> ([unitid]     -> Builder)
                            -> (Int -> modulename -> Builder) -> ([modulename] -> Builder)
                            -> Int -> ExposedModule unitid modulename -> Builder
liftShowbExposedModulePrec2 = liftShowbPrec2
# endif

$(deriveTextShow  ''InstalledPackageInfo)
$(deriveTextShow1 ''InstalledPackageInfo)
$(deriveTextShow2 ''InstalledPackageInfo)
# if __GLASGOW_HASKELL__ >= 801
$(deriveTextShow  ''DbModule)
$(deriveTextShow1 ''DbModule)
$(deriveTextShow2 ''DbModule)
# else
$(deriveTextShow  ''OriginalModule)
$(deriveTextShow1 ''OriginalModule)
$(deriveTextShow2 ''OriginalModule)

$(deriveTextShow  ''ExposedModule)
$(deriveTextShow1 ''ExposedModule)
$(deriveTextShow2 ''ExposedModule)
# endif
#endif
