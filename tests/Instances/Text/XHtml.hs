{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric      #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Text.XHtml
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @xhtml@ library.
-}
module Instances.Text.XHtml () where

#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics (Generic)
#else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
#endif

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), Gen, genericArbitrary)

import           Text.XHtml.Frameset (Html, HtmlAttr, HotLink(..), strAttr, toHtml)
import           Text.XHtml.Table (HtmlTable, cell)

instance Arbitrary Html where
    arbitrary = toHtml <$> (arbitrary :: Gen String)

instance Arbitrary HtmlAttr where
    arbitrary = strAttr <$> arbitrary <*> arbitrary

instance Arbitrary HotLink where
    arbitrary = genericArbitrary

instance Arbitrary HtmlTable where
    arbitrary = cell <$> (arbitrary :: Gen Html)

#if __GLASGOW_HASKELL__ >= 702
deriving instance Generic HotLink
#else
$(Generics.deriveAll0 ''HotLink)
#endif
