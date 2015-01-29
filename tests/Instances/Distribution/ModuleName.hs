{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Compiler
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides an 'Arbitrary' instance for 'ModuleName's.
-}
module Instances.Distribution.ModuleName (fModuleName) where

import Data.Char (isAlphaNum, isUpper)
import Distribution.ModuleName (ModuleName, fromString)
import Test.Tasty.QuickCheck (Arbitrary(..), suchThat)

instance Arbitrary ModuleName where
    arbitrary = do
        moduleName <- arbitrary `suchThat` (all validModuleComponent . split)
        return $ fromString moduleName

split :: String -> [String]
split cs = case break (== '.') cs of
    (chunk, [])     -> chunk : []
    (chunk, _:rest) -> chunk : split rest

validModuleChar :: Char -> Bool
validModuleChar c = isAlphaNum c || c == '_' || c == '\''

validModuleComponent :: String -> Bool
validModuleComponent []     = False
validModuleComponent (c:cs) = isUpper c && all validModuleChar cs

-------------------------------------------------------------------------------
-- Workarounds to make Arbitrary instances faster
-------------------------------------------------------------------------------

fModuleName :: ModuleName
fModuleName = fromString "A"