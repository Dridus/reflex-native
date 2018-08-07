{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Native.ViewLayout.Fill
  ( FillLayout, ContainerLayout(ContainerLayout_Fill), ContentLayout(ContentLayout_Fill)
  ) where

import Data.Default (Default(def))
import Reflex.Native.ViewLayout.Class (ViewLayout(type ContainerLayout, type ContentLayout))


data FillLayout

instance ViewLayout FillLayout t where
  data ContainerLayout FillLayout t = ContainerLayout_Fill
  data ContentLayout FillLayout t = ContentLayout_Fill

instance Default (ContainerLayout FillLayout t) where
  def = ContainerLayout_Fill
instance Default (ContentLayout FillLayout t) where
  def = ContentLayout_Fill
