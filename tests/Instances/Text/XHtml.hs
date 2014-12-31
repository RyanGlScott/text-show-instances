{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Text.XHtml
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @xhtml@ library.
-}
module Instances.Text.XHtml () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>))
#endif

import Data.Functor ((<$>))

import Test.Tasty.QuickCheck (Arbitrary(..), Gen)

import Text.XHtml.Frameset (Html, HtmlAttr, HotLink(..), strAttr, toHtml)
import Text.XHtml.Table (HtmlTable, cell)

instance Arbitrary Html where
    arbitrary = toHtml <$> (arbitrary :: Gen String)

instance Arbitrary HtmlAttr where
    arbitrary = strAttr <$> arbitrary <*> arbitrary

instance Arbitrary HotLink where
    arbitrary = HotLink <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary HtmlTable where
    arbitrary = cell <$> (arbitrary :: Gen Html)