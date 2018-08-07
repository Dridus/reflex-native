{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Native.ViewLayout.Linear
  ( LinearLayout, ColumnLayout, RowLayout
  , ContainerLayout(..)
  , ContentLayout(ContentLayout_Linear)
  ) where

import Data.Default (Default(def))
import Reflex.Native.Geometry (Axis(Horizontal, Vertical))
import Reflex.Native.ViewLayout.Class (ViewLayout(type ContainerLayout, type ContentLayout))


data LinearLayout (axis :: Axis)
type ColumnLayout = LinearLayout 'Vertical
type RowLayout = LinearLayout 'Horizontal

instance ViewLayout (LinearLayout axis) t where
  data ContainerLayout (LinearLayout axis) t = ContainerLayout_Linear
    { _containerLayout_linear_spacingBetween :: !Double
    }
  data ContentLayout (LinearLayout axis) t = ContentLayout_Linear

instance Default (ContainerLayout (LinearLayout axis) t) where
  def = ContainerLayout_Linear 8
instance Default (ContentLayout (LinearLayout axis) t) where
  def = ContentLayout_Linear

