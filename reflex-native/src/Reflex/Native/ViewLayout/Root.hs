{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Native.ViewLayout.Root
  ( RootLayout, ContainerLayout(ContainerLayout_Root), ContentLayout(ContentLayout_Root)
  ) where

import Data.Default (Default(def))
import Reflex.Native.ViewLayout.Class (ViewLayout(type ContainerLayout, type ContentLayout))


data RootLayout

instance ViewLayout t RootLayout where
  data ContainerLayout t RootLayout = ContainerLayout_Root
  data ContentLayout t RootLayout = ContentLayout_Root

instance Default (ContainerLayout t RootLayout) where
  def = ContainerLayout_Root
instance Default (ContentLayout t RootLayout) where
  def = ContentLayout_Root
