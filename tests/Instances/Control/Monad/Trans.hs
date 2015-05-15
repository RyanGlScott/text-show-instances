{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans               #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-|
Module:      Instances.Control.Monad.Trans
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for monad transformers.
-}
module Instances.Control.Monad.Trans () where

import           Control.Monad.Trans.Error               (ErrorT(..))
import           Control.Monad.Trans.Except              (ExceptT(..))
import           Control.Monad.Trans.Identity            (IdentityT(..))
import           Control.Monad.Trans.List                (ListT(..))
import           Control.Monad.Trans.Maybe               (MaybeT(..))
import qualified Control.Monad.Trans.Writer.Lazy   as WL (WriterT(..))
import qualified Control.Monad.Trans.Writer.Strict as WS (WriterT(..))

import           Test.QuickCheck (Arbitrary)

deriving instance Arbitrary (m (Either e a)) => Arbitrary (ErrorT e m a)
deriving instance Arbitrary (m (Either e a)) => Arbitrary (ExceptT e m a)
deriving instance Arbitrary (f a)            => Arbitrary (IdentityT f a)
deriving instance Arbitrary (m [a])          => Arbitrary (ListT m a)
deriving instance Arbitrary (m (Maybe a))    => Arbitrary (MaybeT m a)
deriving instance Arbitrary (m (a, w))       => Arbitrary (WL.WriterT w m a)
deriving instance Arbitrary (m (a, w))       => Arbitrary (WS.WriterT w m a)
