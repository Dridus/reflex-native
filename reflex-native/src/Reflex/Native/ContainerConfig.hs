{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Reflex.Native.ContainerConfig (ContainerConfig(..)) where

import Data.Default (Default(def))
import GHC.Generics (Generic)
import Reflex.Class (Reflex)
import Reflex.Native.ViewConfig (ViewConfig)
import Reflex.Native.ViewLayout.Class (ViewLayout(type ContainerLayout))


data ContainerConfig t layout layout' = ContainerConfig
  { _containerConfig_layout :: ContainerLayout t layout'
  -- ^Layout information for the layout of the contents of the container.
  -- See also @_viewConfig_layout@ for the layout information of a view (container or otherwise) within its parent container.
  , _containerConfig_viewConfig :: ViewConfig t layout
  -- ^The general 'ViewConfig' for the container.
  } deriving (Generic)

-- |Default container configuration which has a default style and lays out its subviews in a column with default spacing.
instance (ViewLayout t layout, ViewLayout t layout', Reflex t) => Default (ContainerConfig t layout layout') where
  def = ContainerConfig
    { _containerConfig_layout     = def
    , _containerConfig_viewConfig = def
    }

