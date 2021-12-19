{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
module TextShow.Data.Aeson where
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.List ( sortBy )
import Data.Ord (comparing)
import TextShow.Data.Scientific ()
import TextShow.Data.Vector ()
import TextShow

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
        <> showbPrec 11 (sortBy (comparing fst) (H.toList xs))
        <> singleton ')'
