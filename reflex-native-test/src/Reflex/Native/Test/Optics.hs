{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
-- |Convenient optics for writing tests.
module Reflex.Native.Test.Optics
  (
  -- * 'TestView' sum
    _Container, _Marker, _Text
  -- * 'TestContainerView'
  , container_contents, subviews
  -- * 'TestTextView'
  , text_text
  ) where

import Control.Lens (Fold, Getter, _Just, to)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Sequence (Seq)
import Data.Text (Text)
import Reflex.Native.Test.Types
  ( SomeTestMarker(..)
  , TestView(..), SomeTestView(..)
  , TestContainerView(..), SomeTestContainerView(..)
  , TestTextView(..), SomeTestTextView(..)
  )


-- |Prism to select a 'TestContainerView' among the constructors of 'TestView'
_Container :: Fold (SomeTestView t v) (SomeTestContainerView t v)
_Container = to f . _Just
  where
    f (SomeTestView (TestView_Container cv)) = Just $ SomeTestContainerView cv
    f _ = Nothing

-- |Prism to select a 'TestMarker' among the constructors of 'TestView'
_Marker :: Fold (SomeTestView t v) (SomeTestMarker t)
_Marker = to f . _Just
  where
    f (SomeTestView (TestView_Marker m)) = Just $ SomeTestMarker m
    f _ = Nothing

-- |Prism to select a 'TestTextView' among the constructors of 'TestView'
_Text :: Fold (SomeTestView t v) (SomeTestTextView v)
_Text = to f . _Just
  where
    f (SomeTestView (TestView_Text tv)) = Just $ SomeTestTextView tv
    f _ = Nothing

-- |Getter projecting out the subviews of a 'SomeTestContainerView' where the inner layout type is unknown as 'SomeTestView's.
container_contents :: Fold (SomeTestContainerView t Identity) (Seq (SomeTestView t Identity))
container_contents = to $ \ (SomeTestContainerView cv) -> SomeTestView <$> runIdentity (_testContainerView_contents cv)

-- |Traversal to visit the contents of any targeted view which happens to be a container
subviews :: Fold (SomeTestView t Identity) (Seq (SomeTestView t Identity))
subviews = _Container . container_contents

-- |Lens to a 'TestTextView's text value.
text_text :: Getter (SomeTestTextView Identity) Text
text_text = to $ \ (SomeTestTextView tv) -> runIdentity (_testTextView_text tv)
