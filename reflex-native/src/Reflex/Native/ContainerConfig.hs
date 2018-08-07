{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Reflex.Native.ContainerConfig (ContainerConfig(..)) where

import Data.Default (Default(def))
import GHC.Generics (Generic)
import Reflex.Class (Reflex)
import Reflex.Native.ViewConfig (ViewConfig)
import Reflex.Native.ViewLayout.Class (ViewLayout(type ContainerLayout))


data ContainerConfig layout layout' t = ContainerConfig
  { _containerConfig_layout :: ContainerLayout layout' t
  -- ^Layout information for the layout of the contents of the container.
  -- See also @_viewConfig_layout@ for the layout information of a view (container or otherwise) within its parent container.
  , _containerConfig_viewConfig :: ViewConfig layout t
  -- ^The general 'ViewConfig' for the container.
  } deriving (Generic)

-- |Default container configuration which has a default style and lays out its subviews in a column with default spacing.
instance (ViewLayout layout t, ViewLayout layout' t, Reflex t) => Default (ContainerConfig layout layout' t) where
  def = ContainerConfig
    { _containerConfig_layout     = def
    , _containerConfig_viewConfig = def
    }

