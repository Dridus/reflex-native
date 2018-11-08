{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
-- |Style parameters for all views.
module Reflex.Native.ViewStyle (ViewStyle(..)) where

import Data.Default (Default(def))
import Data.Functor.Identity (Identity(..))
import GHC.Generics (Generic)
import qualified Rank2
import Rank2 (apply)
import Reflex.Class (Event, Reflex, never)
import Reflex.Native.Color (Color, clear)


-- |Style of displayed view parameterized over functor @f@.
--
-- @f ~ Identity@ is used for initial view style where all parameters must be given, while @f ~ Event t@ is used for dynamic modification of view styles after
data ViewStyle f = ViewStyle
  { _viewStyle_backgroundColor :: f Color
  -- ^Background color to draw the view with.
  } deriving (Generic)

instance Rank2.Functor ViewStyle where
  f <$> ViewStyle a = ViewStyle (f a)
instance Rank2.Apply ViewStyle where
  ViewStyle fa <*> ViewStyle a = ViewStyle (apply fa a)
instance Rank2.Applicative ViewStyle where
  pure f = ViewStyle f
instance Rank2.Foldable ViewStyle where
  foldMap f (ViewStyle a) = f a
instance Rank2.Traversable ViewStyle where
  traverse f (ViewStyle a) = ViewStyle <$> f a

-- |Default 'ViewStyle' for initial display of a view: a transparent background.
instance Default (ViewStyle Identity) where
  def = ViewStyle
    { _viewStyle_backgroundColor = Identity clear
    }

-- |Default 'ViewStyle' for dynamic update, where all parameters 'never' update.
instance Reflex t => Default (ViewStyle (Event t)) where
  def = Rank2.pure never

