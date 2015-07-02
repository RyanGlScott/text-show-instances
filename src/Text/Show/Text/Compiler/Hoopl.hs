{-# LANGUAGE CPP               #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Compiler.Hoopl
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' functions for data types in the @hoopl@ library.

/Since: 0.2/
-}
module Text.Show.Text.Compiler.Hoopl (
      showbLabel
    , showbLabelMapPrecWith
    , showbLabelSetPrec
    , showbPointedWith
    , showbUnique
    , showbUniqueMapPrecWith
    , showbUniqueSetPrec
    , showbDominatorNode
    , showbDominatorTree
    , showbDPath
    ) where

import Compiler.Hoopl (Label, LabelMap, LabelSet, Pointed(..),
                       Unique, UniqueMap, UniqueSet)
#if MIN_VERSION_hoopl(3,9,0)
import Compiler.Hoopl.Internals (lblToUnique)
#else
import Compiler.Hoopl.GHC (lblToUnique, uniqueToInt)
#endif
import Compiler.Hoopl.Passes.Dominator (DominatorNode(..), DominatorTree(..), DPath(..))

import Data.Monoid.Compat

import Prelude hiding (Show)

import Text.Show.Text (Show(showb, showbPrec), Show1(..), Show2(..),
                       Builder, singleton)
import Text.Show.Text.Data.Containers ()
import Text.Show.Text.Data.Integral (showbIntPrec)
import Text.Show.Text.TH (deriveShow, deriveShow1)

#include "inline.h"

-- | Convert a 'Label' to a 'Builder'.
--
-- /Since: 0.2/
showbLabel :: Label -> Builder
showbLabel l = singleton 'L' <> showbUnique (lblToUnique l)
{-# INLINE showbLabel #-}

-- | Convert a 'LabelMap' to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbLabelMapPrecWith :: (Int -> v -> Builder) -> Int -> LabelMap v -> Builder
showbLabelMapPrecWith = showbPrecWith
{-# INLINE showbLabelMapPrecWith #-}

-- | Convert a 'LabelSet' to a 'Builder' with the given precedence.
--
-- /Since: 0.2/
showbLabelSetPrec :: Int -> LabelSet -> Builder
showbLabelSetPrec = showbPrec
{-# INLINE showbLabelSetPrec #-}

-- | Convert a 'Pointed' value to a 'Builder' with the given show function.
--
-- /Since: 1/
showbPointedWith :: (a -> Builder) -> Pointed t b a -> Builder
showbPointedWith _  Bot       = "_|_"
showbPointedWith _  Top       = singleton 'T'
showbPointedWith sp (PElem a) = sp a
{-# INLINE showbPointedWith #-}

-- | Convert a 'Unique' value to a 'Builder'.
--
-- /Since: 0.2/
showbUnique :: Unique -> Builder
#if MIN_VERSION_hoopl(3,9,0)
showbUnique = showbIntPrec 0
#else
showbUnique = showbIntPrec 0 . uniqueToInt
#endif
{-# INLINE showbUnique #-}

-- | Convert a 'UniqueMap' to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbUniqueMapPrecWith :: (Int -> v -> Builder) -> Int -> UniqueMap v -> Builder
showbUniqueMapPrecWith = showbPrecWith
{-# INLINE showbUniqueMapPrecWith #-}

-- | Convert a 'UniqueSet' to a 'Builder' with the given precedence.
--
-- /Since: 0.2/
showbUniqueSetPrec :: Int -> UniqueSet -> Builder
showbUniqueSetPrec = showbPrec
{-# INLINE showbUniqueSetPrec #-}

-- | Convert a 'DominatorNode' to a 'Builder'.
--
-- /Since: 0.2/
showbDominatorNode :: DominatorNode -> Builder
showbDominatorNode Entry        = "entryNode"
showbDominatorNode (Labelled l) = showbLabel l
{-# INLINE showbDominatorNode #-}

-- | Convert a 'DominatorTree' to a 'Builder'.
--
-- /Since: 0.2/
showbDominatorTree :: DominatorTree -> Builder
showbDominatorTree t = mconcat $ "digraph {\n" : dot t ["}\n"]
  where
    dot :: DominatorTree -> [Builder] -> [Builder]
    dot (Dominates root trees) =
        (dotnode root :) . outedges trees . flip (foldl subtree) trees
      where
        outedges :: [DominatorTree] -> [Builder] -> [Builder]
        outedges [] = id
        outedges (Dominates n _ : ts) =
              \bs -> "  "
            : showbDominatorNode root
            : " -> "
            : showbDominatorNode n
            : singleton '\n'
            : outedges ts bs

        dotnode :: DominatorNode -> Builder
        dotnode Entry        = "  entryNode [shape=plaintext, label=\"entry\"]\n"
        dotnode (Labelled l) = "  " <> showbLabel l <> singleton '\n'

        subtree :: [Builder] -> DominatorTree -> [Builder]
        subtree = flip dot

-- | Convert a 'DPath' to a 'Builder'.
--
-- /Since: 0.2/
showbDPath :: DPath -> Builder
showbDPath (DPath ls) = mconcat $ foldr (\l path ->showbLabel l <> " -> " : path)
                                        ["entry"]
                                        ls
{-# INLINE showbDPath #-}

instance Show Label where
    showb = showbLabel
    INLINE_INST_FUN(showb)

$(deriveShow  ''LabelMap)
$(deriveShow1 ''LabelMap)

$(deriveShow  ''LabelSet)

instance Show a => Show (Pointed t b a) where
    showbPrec = showbPrecWith showbPrec
    INLINE_INST_FUN(showbPrec)

instance Show1 (Pointed t b) where
    showbPrecWith sp _ = showbPointedWith $ sp 0
    INLINE_INST_FUN(showbPrecWith)

instance Show2 (Pointed t) where
    showbPrecWith2 _ = showbPrecWith
    INLINE_INST_FUN(showbPrecWith2)

#if !(MIN_VERSION_hoopl(3,9,0))
instance Show Unique where
    showb = showbUnique
    INLINE_INST_FUN(showb)
#endif

$(deriveShow  ''UniqueMap)
$(deriveShow1 ''UniqueMap)

$(deriveShow  ''UniqueSet)

instance Show DominatorNode where
    showb = showbDominatorNode
    INLINE_INST_FUN(showb)

instance Show DominatorTree where
    showb = showbDominatorTree
    INLINE_INST_FUN(showb)

instance Show DPath where
    showb = showbDPath
    INLINE_INST_FUN(showb)
