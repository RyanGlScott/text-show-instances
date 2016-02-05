{-# LANGUAGE CPP               #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Compiler.Hoopl
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @hoopl@ library.

/Since: 2/
-}
module TextShow.Compiler.Hoopl (
      showbLabel
    , liftShowbLabelMapPrec
    , showbLabelSetPrec
    , liftShowbPointed
    , showbUnique
    , liftShowbUniqueMapPrec
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

import TextShow (TextShow(..), TextShow1(..),
                 TextShow2(..), Builder, singleton, showbPrec1)
import TextShow.Data.Containers ()
import TextShow.Data.Integral (showbIntPrec)
import TextShow.TH (deriveTextShow, deriveTextShow1)

#include "inline.h"

-- | Convert a 'Label' to a 'Builder'.
--
-- /Since: 2/
showbLabel :: Label -> Builder
showbLabel l = singleton 'L' <> showbUnique (lblToUnique l)
{-# INLINE showbLabel #-}

-- | Convert a 'LabelMap' to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbLabelMapPrec :: (v -> Builder) -> Int -> LabelMap v -> Builder
liftShowbLabelMapPrec sp = liftShowbPrec (const sp) undefined
{-# INLINE liftShowbLabelMapPrec #-}

-- | Convert a 'LabelSet' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbLabelSetPrec :: Int -> LabelSet -> Builder
showbLabelSetPrec = showbPrec
{-# INLINE showbLabelSetPrec #-}

-- | Convert a 'Pointed' value to a 'Builder' with the given show function.
--
-- /Since: 3/
liftShowbPointed :: (a -> Builder) -> Pointed t b a -> Builder
liftShowbPointed _  Bot       = "_|_"
liftShowbPointed _  Top       = singleton 'T'
liftShowbPointed sp (PElem a) = sp a
{-# INLINE liftShowbPointed #-}

-- | Convert a 'Unique' value to a 'Builder'.
--
-- /Since: 2/
showbUnique :: Unique -> Builder
#if MIN_VERSION_hoopl(3,9,0)
showbUnique = showbIntPrec 0
#else
showbUnique = showbIntPrec 0 . uniqueToInt
#endif
{-# INLINE showbUnique #-}

-- | Convert a 'UniqueMap' to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbUniqueMapPrec :: (v -> Builder) -> Int -> UniqueMap v -> Builder
liftShowbUniqueMapPrec sp = liftShowbPrec (const sp) undefined
{-# INLINE liftShowbUniqueMapPrec #-}

-- | Convert a 'UniqueSet' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbUniqueSetPrec :: Int -> UniqueSet -> Builder
showbUniqueSetPrec = showbPrec
{-# INLINE showbUniqueSetPrec #-}

-- | Convert a 'DominatorNode' to a 'Builder'.
--
-- /Since: 2/
showbDominatorNode :: DominatorNode -> Builder
showbDominatorNode Entry        = "entryNode"
showbDominatorNode (Labelled l) = showbLabel l
{-# INLINE showbDominatorNode #-}

-- | Convert a 'DominatorTree' to a 'Builder'.
--
-- /Since: 2/
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
-- /Since: 2/
showbDPath :: DPath -> Builder
showbDPath (DPath ls) = mconcat $ foldr (\l path ->showbLabel l <> " -> " : path)
                                        ["entry"]
                                        ls
{-# INLINE showbDPath #-}

instance TextShow Label where
    showb = showbLabel
    INLINE_INST_FUN(showb)

$(deriveTextShow  ''LabelMap)
$(deriveTextShow1 ''LabelMap)

$(deriveTextShow  ''LabelSet)

instance TextShow a => TextShow (Pointed t b a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 (Pointed t b) where
    liftShowbPrec sp _ _ = liftShowbPointed $ sp 0
    INLINE_INST_FUN(liftShowbPrec)

instance TextShow2 (Pointed t) where
    liftShowbPrec2 _ _ = liftShowbPrec
    INLINE_INST_FUN(liftShowbPrec2)

#if !(MIN_VERSION_hoopl(3,9,0))
instance TextShow Unique where
    showb = showbUnique
    INLINE_INST_FUN(showb)
#endif

$(deriveTextShow  ''UniqueMap)
$(deriveTextShow1 ''UniqueMap)

$(deriveTextShow  ''UniqueSet)

instance TextShow DominatorNode where
    showb = showbDominatorNode
    INLINE_INST_FUN(showb)

instance TextShow DominatorTree where
    showb = showbDominatorTree
    INLINE_INST_FUN(showb)

instance TextShow DPath where
    showb = showbDPath
    INLINE_INST_FUN(showb)
