{-# LANGUAGE CPP, GADTs, OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Compiler.Hoopl
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @hoopl@ library.

/Since: 0.1/
-}
module Text.Show.Text.Compiler.Hoopl (
      showbLabel
    , showbLabelMapPrec
    , showbLabelSetPrec
    , showbPointed
    , showbUniqueMapPrec
    , showbUniqueSetPrec
    , showbDominatorNode
    , showbDominatorTree
    , showbDPath
    ) where

import Compiler.Hoopl (Label, LabelMap, LabelSet, Pointed(..), UniqueMap, UniqueSet)
#if MIN_VERSION_hoopl(3,9,0)
import Compiler.Hoopl.Internals (lblToUnique)
#else
import Compiler.Hoopl.GHC (lblToUnique)
#endif
import Compiler.Hoopl.Passes.Dominator (DominatorNode(..), DominatorTree(..), DPath(..))

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (mconcat)
#endif

import Prelude hiding (Show)

import Text.Show.Text (Show(showb, showbPrec), Show1(showbPrec1), Builder)
import Text.Show.Text.Data.Containers ()
import Text.Show.Text.Data.Integral (showbIntPrec)
import Text.Show.Text.TH (deriveShowPragmas, defaultInlineShowbPrec)
import Text.Show.Text.Utils ((<>), s)

#include "inline.h"

-- | Convert a 'Label' to a 'Builder'.
showbLabel :: Label -> Builder
showbLabel l = s 'L' <> showbIntPrec 0 (lblToUnique l)
{-# INLINE showbLabel #-}

-- | Convert a 'LabelMap' to a 'Builder' with the given precedence.
showbLabelMapPrec :: Show v => Int -> LabelMap v -> Builder
showbLabelMapPrec = showbPrec
{-# INLINE showbLabelMapPrec #-}

-- | Convert a 'LabelSet' to a 'Builder' with the given precedence.
showbLabelSetPrec :: Int -> LabelSet -> Builder
showbLabelSetPrec = showbPrec
{-# INLINE showbLabelSetPrec #-}

-- | Convert a 'Pointed' value to a 'Builder'.
showbPointed :: Show a => Pointed t b a -> Builder
showbPointed Bot       = "_|_"
showbPointed Top       = s 'T'
showbPointed (PElem a) = showb a
{-# INLINE showbPointed #-}

-- | Convert a 'UniqueMap' to a 'Builder' with the given precedence.
showbUniqueMapPrec :: Show v => Int -> UniqueMap v -> Builder
showbUniqueMapPrec = showbPrec
{-# INLINE showbUniqueMapPrec #-}

-- | Convert a 'UniqueSet' to a 'Builder' with the given precedence.
showbUniqueSetPrec :: Int -> UniqueSet -> Builder
showbUniqueSetPrec = showbPrec
{-# INLINE showbUniqueSetPrec #-}

-- | Convert a 'DominatorNode' to a 'Builder'.
showbDominatorNode :: DominatorNode -> Builder
showbDominatorNode Entry        = "entryNode"
showbDominatorNode (Labelled l) = showbLabel l
{-# INLINE showbDominatorNode #-}

-- | Convert a 'DominatorTree' to a 'Builder'.
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
            : s '\n'
            : outedges ts bs
        
        dotnode :: DominatorNode -> Builder
        dotnode Entry        = "  entryNode [shape=plaintext, label=\"entry\"]\n"
        dotnode (Labelled l) = "  " <> showbLabel l <> s '\n'
        
        subtree :: [Builder] -> DominatorTree -> [Builder]
        subtree = flip dot

showbDPath :: DPath -> Builder
showbDPath (DPath ls) = mconcat $ foldr (\l path ->showbLabel l <> " -> " : path)
                                        ["entry"]
                                        ls
{-# INLINE showbDPath #-}

instance Show Label where
    showb = showbLabel
    INLINE_INST_FUN(showb)

$(deriveShowPragmas defaultInlineShowbPrec ''LabelMap)

instance Show1 LabelMap where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

$(deriveShowPragmas defaultInlineShowbPrec ''LabelSet)

instance Show a => Show (Pointed t b a) where
    showb = showbPointed
    INLINE_INST_FUN(showb)

instance Show1 (Pointed t b) where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

$(deriveShowPragmas defaultInlineShowbPrec ''UniqueMap)

instance Show1 UniqueMap where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

$(deriveShowPragmas defaultInlineShowbPrec ''UniqueSet)

instance Show DominatorNode where
    showb = showbDominatorNode
    INLINE_INST_FUN(showb)

instance Show DominatorTree where
    showb = showbDominatorTree
    INLINE_INST_FUN(showb)

instance Show DPath where
    showb = showbDPath
    INLINE_INST_FUN(showb)