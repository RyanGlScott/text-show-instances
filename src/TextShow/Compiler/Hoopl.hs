{-# LANGUAGE CPP               #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Compiler.Hoopl
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @hoopl@ library.

/Since: 2/
-}
module TextShow.Compiler.Hoopl () where

import Compiler.Hoopl (Label, LabelMap, LabelSet, Pointed(..),
                       UniqueMap, UniqueSet)
#if MIN_VERSION_hoopl(3,9,0)
import Compiler.Hoopl.Internals (lblToUnique)
#else
import Compiler.Hoopl (Unique)
import Compiler.Hoopl.GHC (lblToUnique, uniqueToInt)
#endif
import Compiler.Hoopl.Passes.Dominator (DominatorNode(..), DominatorTree(..), DPath(..))

import Prelude ()
import Prelude.Compat

import TextShow (TextShow(..), TextShow1(..),
                 TextShow2(..), Builder, singleton, showbPrec1)
import TextShow.Data.Containers ()
import TextShow.TH (deriveTextShow, deriveTextShow1)

-- | /Since: 2/
instance TextShow Label where
    showb l = singleton 'L' <> showb (lblToUnique l)
    {-# INLINE showb #-}

-- | /Since: 2/
$(deriveTextShow  ''LabelMap)
-- | /Since: 2/
$(deriveTextShow1 ''LabelMap)

-- | /Since: 2/
$(deriveTextShow  ''LabelSet)

-- | /Since: 2/
instance TextShow a => TextShow (Pointed t b a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 (Pointed t b) where
    liftShowbPrec _  _ _ Bot       = "_|_"
    liftShowbPrec _  _ _ Top       = singleton 'T'
    liftShowbPrec sp _ _ (PElem a) = sp 0 a
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance TextShow2 (Pointed t) where
    liftShowbPrec2 _ _ = liftShowbPrec
    {-# INLINE liftShowbPrec2 #-}

#if !(MIN_VERSION_hoopl(3,9,0))
-- | /Since: 2/
instance TextShow Unique where
    showb = showb . uniqueToInt
    {-# INLINE showb #-}
#endif

-- | /Since: 2/
$(deriveTextShow  ''UniqueMap)
-- | /Since: 2/
$(deriveTextShow1 ''UniqueMap)

-- | /Since: 2/
$(deriveTextShow  ''UniqueSet)

-- | /Since: 2/
instance TextShow DominatorNode where
    showb Entry        = "entryNode"
    showb (Labelled l) = showb l
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow DominatorTree where
    showb t = mconcat $ "digraph {\n" : dot t ["}\n"]
      where
        dot :: DominatorTree -> [Builder] -> [Builder]
        dot (Dominates root trees) =
            (dotnode root :) . outedges trees . flip (foldl subtree) trees
          where
            outedges :: [DominatorTree] -> [Builder] -> [Builder]
            outedges [] = id
            outedges (Dominates n _ : ts) =
                  \bs -> "  "
                : showb root
                : " -> "
                : showb n
                : singleton '\n'
                : outedges ts bs

            dotnode :: DominatorNode -> Builder
            dotnode Entry        = "  entryNode [shape=plaintext, label=\"entry\"]\n"
            dotnode (Labelled l) = "  " <> showb l <> singleton '\n'

            subtree :: [Builder] -> DominatorTree -> [Builder]
            subtree = flip dot

-- | /Since: 2/
instance TextShow DPath where
    showb (DPath ls) = mconcat $ foldr (\l path -> showb l <> " -> " : path)
                                       ["entry"]
                                       ls
    {-# INLINE showb #-}
