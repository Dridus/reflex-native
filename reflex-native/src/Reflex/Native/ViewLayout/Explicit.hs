{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Native.ViewLayout.Explicit
  ( ExplicitLayout, ContainerLayout(ContainerLayout_Explicit), ContentLayout(..)
  ) where

import Data.AdditiveGroup (AdditiveGroup(zeroV))
import Data.Default (Default(def))
import Reflex.Class (Event)
import Reflex.Native.Geometry (Rect(..))
import Reflex.Native.ViewLayout.Class (ViewLayout(type ContainerLayout, type ContentLayout))


data ExplicitLayout

instance ViewLayout ExplicitLayout t where
  data ContainerLayout ExplicitLayout t = ContainerLayout_Explicit
  data ContentLayout ExplicitLayout t = ContentLayout_Explicit
    { _contentLayout_explicit_initialRect :: Rect
    , _contentLayout_explicit_setRect :: Maybe (Event t Rect)
    }

instance Default (ContainerLayout ExplicitLayout t) where
  def = ContainerLayout_Explicit
instance Default (ContentLayout ExplicitLayout t) where
  def = ContentLayout_Explicit (Rect zeroV zeroV) Nothing

