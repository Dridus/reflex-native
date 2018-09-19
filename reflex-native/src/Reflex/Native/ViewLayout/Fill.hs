{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Native.ViewLayout.Fill
  ( FillLayout, ContainerLayout(ContainerLayout_Fill), ContentLayout(ContentLayout_Fill)
  ) where

import Data.Default (Default(def))
import Reflex.Native.ViewLayout.Class (ViewLayout(type ContainerLayout, type ContentLayout))


data FillLayout

instance ViewLayout t FillLayout where
  data ContainerLayout t FillLayout = ContainerLayout_Fill
  data ContentLayout t FillLayout = ContentLayout_Fill

instance Default (ContainerLayout t FillLayout) where
  def = ContainerLayout_Fill
instance Default (ContentLayout t FillLayout) where
  def = ContentLayout_Fill
