{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- |Configuration of a text display view.
module Reflex.Native.TextConfig
  ( TextConfig(..)
  ) where

import Data.Default (Default(def))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe(Nothing))
import Data.Text (Text)
import GHC.Generics (Generic)
import Reflex.Class (Event, Reflex)
import Reflex.Native.TextStyle (TextStyle)
import Reflex.Native.ViewConfig (ViewConfig)
import Reflex.Native.ViewLayout.Class (ViewLayout)


-- |Configuration of a text view using Reflex timeline @t@.
data TextConfig t layout = TextConfig
  { _textConfig_initialText  :: Text
  -- ^The initial text displayed which can be updated dynamically by providing a @_textConfig_setText@ @Event@.
  , _textConfig_setText      :: Maybe (Event t Text)
  -- ^An @Event@ which updates the displayed text each time it fires.
  , _textConfig_initialStyle :: TextStyle Identity
  -- ^The initial 'TextStyle' to apply to the displayed text.
  , _textConfig_modifyStyle  :: Maybe (TextStyle (Event t))
  -- ^A 'TextStyle' where each parameter is an @Event@ which dynamically updates the associated style of the displayed text when it fires.
  , _textConfig_viewConfig   :: ViewConfig t layout
  -- ^The general 'ViewConfig' for the view.
  } deriving (Generic)

-- |Default text configuration which displays no text, has a default style, and never updates.
instance (ViewLayout t layout, Reflex t) => Default (TextConfig t layout) where
  def = TextConfig
    { _textConfig_initialText  = ""
    , _textConfig_setText      = Nothing
    , _textConfig_initialStyle = def
    , _textConfig_modifyStyle  = Nothing
    , _textConfig_viewConfig   = def
    }

