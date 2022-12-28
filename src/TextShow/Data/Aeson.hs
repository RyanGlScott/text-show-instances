{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
module TextShow.Data.Aeson where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

import Prelude ()
import Prelude.Compat

import TextShow (TextShow(..), fromText, showbParen, showtToShowb, singleton)
import TextShow.Data.Scientific ()
import TextShow.Data.Vector ()

instance TextShow K.Key where
    showb = showtToShowb showt
    showt = showt . K.toText

instance TextShow Value where
    showbPrec _ Null = fromText "Null"
    showbPrec d (Bool b) = showbParen (d > 10)
        $ fromText "Bool " <> showbPrec 11 b
    showbPrec d (Number s) = showbParen (d > 10)
        $ fromText "Number " <> showbPrec 11 s
    showbPrec d (String s) = showbParen (d > 10)
        $ fromText "String " <> showbPrec 11 s
    showbPrec d (Array xs) = showbParen (d > 10)
        $ fromText "Array " <> showbPrec 11 xs
    showbPrec d (Object xs) = showbParen (d > 10)
        $ fromText "Object (fromList "
        <> showbPrec 11 (KM.toAscList xs)
        <> singleton ')'
