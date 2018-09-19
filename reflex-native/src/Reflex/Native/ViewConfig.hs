{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
-- |Configuration of any type of view.
module Reflex.Native.ViewConfig
  (
    -- * Configuration for regular views created with Reflex Native
    ViewConfig(..)
    -- * Configuration for other views adopted by Reflex Native
  , RawViewConfig(..), viewConfigToRawViewConfig
  ) where

import Data.Default (Default(def))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe(Nothing))
import Data.Text (Text)
import GHC.Generics (Generic)
import Reflex.Class (Event, Reflex)
import Reflex.Native.ViewLayout.Class (ViewLayout(type ContentLayout))
import Reflex.Native.ViewStyle (ViewStyle)


-- |Configuration of any type of view created by Reflex native, including its style, layout, and accessibility parameters.
data ViewConfig t layout = ViewConfig
  { _viewConfig_initialStyle :: ViewStyle Identity
  -- ^Style to initially use when displaying the view.
  , _viewConfig_modifyStyle :: Maybe (ViewStyle (Event t))
  -- ^Optional @Event@s to dynamically update the view style after initial display.
  , _viewConfig_initialAccessibilityLabel :: Maybe Text
  -- ^Initial accessibility label to apply to the view.
  , _viewConfig_setAccessibilityLabel :: Maybe (Event t (Maybe Text))
  -- ^Optional @Event@ to dynamically update accessiblity label after initial display.
  , _viewConfig_layout :: ContentLayout t layout
  } deriving (Generic)

-- |Default 'ViewConfig' with the default initial view style, default content layout, and no dynamically updating anything.
instance (Reflex t, ViewLayout t layout) => Default (ViewConfig t layout) where
  def = ViewConfig
    { _viewConfig_initialStyle              = def
    , _viewConfig_modifyStyle               = Nothing
    , _viewConfig_initialAccessibilityLabel = Nothing
    , _viewConfig_setAccessibilityLabel     = Nothing
    , _viewConfig_layout                    = def
    }

-- |Configuration of a raw view created outside Reflex Native and then adopted using 'Reflex.Native.ViewBuilder.Class.wrapRawView' or similar. Allows dynamic
-- update of a view just like 'ViewConfig', but not the initial setting.
data RawViewConfig t = RawViewConfig
  { _rawViewConfig_modifyStyle :: Maybe (ViewStyle (Event t))
  -- ^Optional @Event@s to dynamically update the view style after initial display.
  , _rawViewConfig_setAccessibilityLabel :: Maybe (Event t (Maybe Text))
  -- ^Optional @Event@ to dynamically update accessiblity label after initial display.
  } deriving (Generic)

-- |Default 'RawViewConfig' which never dynamically updates anything.
instance Default (RawViewConfig t) where
  def = RawViewConfig
    { _rawViewConfig_modifyStyle           = Nothing
    , _rawViewConfig_setAccessibilityLabel = Nothing
    }

-- |Extract the equivalent 'RawViewConfig' for some 'ViewConfig'.
viewConfigToRawViewConfig :: ViewConfig t layout -> RawViewConfig t
viewConfigToRawViewConfig (ViewConfig {..}) = RawViewConfig
  { _rawViewConfig_modifyStyle           = _viewConfig_modifyStyle
  , _rawViewConfig_setAccessibilityLabel = _viewConfig_setAccessibilityLabel
  }

