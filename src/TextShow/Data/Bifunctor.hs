{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds            #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Bifunctor
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @bifunctors@ library.

/Since: 2/
-}
module TextShow.Data.Bifunctor () where

import Data.Bifunctor.Biff (Biff)
import Data.Bifunctor.Clown (Clown)
import Data.Bifunctor.Fix (Fix(..))
import Data.Bifunctor.Flip (Flip)
import Data.Bifunctor.Join (Join(..))
import Data.Bifunctor.Joker (Joker)
import Data.Bifunctor.Product (Product)
import Data.Bifunctor.Sum (Sum)
import Data.Bifunctor.Tannen (Tannen)
import Data.Bifunctor.Wrapped (WrappedBifunctor)

import TextShow (TextShow(..), TextShow1(..), TextShow2(..))
import TextShow.TH (deriveTextShow2, makeShowbPrec, makeLiftShowbPrec)

-- | /Since: 2/
instance TextShow (p (f a) (g b)) => TextShow (Biff p f g a b) where
    showbPrec = $(makeShowbPrec ''Biff)
-- | /Since: 2/
instance (TextShow2 p, TextShow1 f, TextShow1 g, TextShow a) => TextShow1 (Biff p f g a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
-- | /Since: 2/
$(deriveTextShow2 ''Biff)

-- | /Since: 2/
instance TextShow (f a) => TextShow (Clown f a b) where
    showbPrec = $(makeShowbPrec ''Clown)
-- | /Since: 2/
instance TextShow (f a) => TextShow1 (Clown f a) where
    liftShowbPrec = $(makeLiftShowbPrec ''Clown)
-- | /Since: 2/
$(deriveTextShow2 ''Clown)

-- | /Since: 2/
instance TextShow (p (Fix p a) a) => TextShow (Fix p a) where
    showbPrec = $(makeShowbPrec ''Fix)
-- | /Since: 2/
instance TextShow2 p => TextShow1 (Fix p) where
    liftShowbPrec sp sl p =
        liftShowbPrec2 (liftShowbPrec sp sl) (liftShowbList sp sl) sp sl p . out

-- | /Since: 2/
instance TextShow (p b a) => TextShow (Flip p a b) where
    showbPrec = $(makeShowbPrec ''Flip)
-- | /Since: 2/
instance (TextShow2 p, TextShow a) => TextShow1 (Flip p a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
-- | /Since: 2/
$(deriveTextShow2 ''Flip)

-- | /Since: 2/
instance TextShow (p a a) => TextShow (Join p a) where
    showbPrec = $(makeShowbPrec ''Join)
-- | /Since: 2/
instance TextShow2 p => TextShow1 (Join p) where
    liftShowbPrec sp sl p = liftShowbPrec2 sp sl sp sl p . runJoin

-- | /Since: 2/
instance TextShow (g b) => TextShow (Joker g a b) where
    showbPrec = $(makeShowbPrec ''Joker)
-- | /Since: 2/
instance TextShow1 g => TextShow1 (Joker g a) where
    liftShowbPrec = $(makeLiftShowbPrec ''Joker)
-- | /Since: 2/
$(deriveTextShow2 ''Joker)

-- | /Since: 2/
instance (TextShow (f a b), TextShow (g a b)) => TextShow (Product f g a b) where
    showbPrec = $(makeShowbPrec ''Product)
-- | /Since: 2/
instance (TextShow2 f, TextShow2 g, TextShow a) => TextShow1 (Product f g a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
-- | /Since: 2/
$(deriveTextShow2 ''Product)

-- | /Since: 2/
instance (TextShow (f a b), TextShow (g a b)) => TextShow (Sum f g a b) where
    showbPrec = $(makeShowbPrec ''Sum)
-- | /Since: 2/
instance (TextShow2 f, TextShow2 g, TextShow a) => TextShow1 (Sum f g a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
-- | /Since: 2/
$(deriveTextShow2 ''Sum)

-- | /Since: 2/
instance TextShow (f (p a b)) => TextShow (Tannen f p a b) where
    showbPrec = $(makeShowbPrec ''Tannen)
-- | /Since: 2/
instance (TextShow1 f, TextShow2 p, TextShow a) => TextShow1 (Tannen f p a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
-- | /Since: 2/
$(deriveTextShow2 ''Tannen)

-- | /Since: 2/
instance TextShow (p a b) => TextShow (WrappedBifunctor p a b) where
    showbPrec = $(makeShowbPrec ''WrappedBifunctor)
-- | /Since: 2/
instance (TextShow2 p, TextShow a) => TextShow1 (WrappedBifunctor p a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
-- | /Since: 2/
$(deriveTextShow2 ''WrappedBifunctor)
