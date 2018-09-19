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

instance ViewLayout t ExplicitLayout where
  data ContainerLayout t ExplicitLayout = ContainerLayout_Explicit
  data ContentLayout t ExplicitLayout = ContentLayout_Explicit
    { _contentLayout_explicit_initialRect :: Rect
    , _contentLayout_explicit_setRect :: Maybe (Event t Rect)
    }

instance Default (ContainerLayout t ExplicitLayout) where
  def = ContainerLayout_Explicit
instance Default (ContentLayout t ExplicitLayout) where
  def = ContentLayout_Explicit (Rect zeroV zeroV) Nothing

