{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- |Cross platform building of view hierarchies with reactive style, layout, and hierarchy.
module Reflex.Native.ViewBuilder.Class where

import Control.Monad (Monad)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.State.Strict (get, put, runStateT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Kind (Constraint)
import Data.Monoid (Monoid)
import Data.Semigroup (Semigroup)
import Reflex.Adjustable.Class (Adjustable)
import Reflex.Class (Event, MonadHold, Reflex)
import Reflex.DynamicWriter.Base (DynamicWriterT(..))
import Reflex.EventWriter.Base (EventWriterT(..))
import Reflex.Native.ContainerConfig (ContainerConfig)
import Reflex.Native.Geometry (Axis(Horizontal, Vertical))
import Reflex.Native.Gesture (GestureData, GestureSpec, GestureState)
import Reflex.Native.TextConfig (TextConfig)
import Reflex.Native.ViewConfig (RawViewConfig)
import Reflex.Native.ViewLayout.Class (ViewLayout(type ContentLayout))
import Reflex.Native.ViewLayout.Fill (FillLayout)
import Reflex.Native.ViewLayout.Linear (LinearLayout)
import Reflex.NotReady.Class (NotReady)
import Reflex.Patch (Additive, Group)
import Reflex.PerformEvent.Class (PerformEvent)
import Reflex.PostBuild.Base (PostBuildT(..))
import Reflex.Query.Base (QueryT(..))
import Reflex.Query.Class (Query)
import Reflex.Requester.Base (RequesterT(..))


-- |Class of types which denote a particular "view space" or particular underlying view system in use, e.g. @UIKitViewSpace@, @TestViewSpace@, or
-- @AndroidViewSpace@, which associate types for each of the standard view types in that underlying view system.
--
-- Each view space has raw types of the various cross-platform notions such as container views and text views, along with a raw arbitrary view type to which
-- the other types can be converted.
class
    ( ViewSpaceSupportsLayout t space FillLayout
    , ViewSpaceSupportsLayout t space (LinearLayout 'Horizontal)
    , ViewSpaceSupportsLayout t space (LinearLayout 'Vertical)
    ) => ViewSpace t space where
  type ViewSpaceSupportsLayout t space :: * -> Constraint

  -- |The type of container views in the underlying view system, e.g. @UIView@ on UIKit or @ViewGroup@ on Android.
  type RawContainerView t space layout layout' :: *

  containerViewAsView :: ContainerView t space layout layout' -> View t space layout

  -- |The type of text views in the underlying view system, e.g. @UILabel@ on UIKit or @TextView@ on Android.
  type RawTextView t space layout :: *

  textViewAsView :: TextView t space layout -> View t space layout

  -- |The type of any arbitrary view in the underlying view system that can be installed via 'placeRawView' or installed and made Reflex-aware using
  -- 'wrapRawView', e.g. @UIView@ on UIKit or @View@ on Android.
  type RawView t space layout :: *

-- |Wrapper around a 'RawContainerView' for a given 'ViewSpace'.
newtype ContainerView t space layout layout' = ContainerView { _containerView_raw :: RawContainerView t space layout layout' }
deriving instance Eq (RawContainerView t space layout layout') => Eq (ContainerView t space layout layout')
deriving instance Show (RawContainerView t space layout layout') => Show (ContainerView t space layout layout')

-- |Wrapper around a 'RawTextView' for a given 'ViewSpace'.
newtype TextView t space layout = TextView { _textView_raw :: RawTextView t space layout }
deriving instance Eq (RawTextView t space layout) => Eq (TextView t space layout)
deriving instance Show (RawTextView t space layout) => Show (TextView t space layout)

-- |Wrapper around a 'RawView' for a given 'ViewSpace'.
newtype View t space layout = View { _view_raw :: RawView t space layout }
deriving instance Eq (RawView t space layout) => Eq (View t space layout)
deriving instance Show (RawView t space layout) => Show (View t space layout)

type family ViewBuilderForLayout layout (m :: * -> *) :: * -> *

-- |The associated 'ViewSpace' for a builder monad.
type family ViewBuilderSpace (m :: * -> *) :: *

-- |Typeclass for monads used to build view hierarchies which react over time to events in a cross-platform way. A function being polymorphic over
-- @ViewBuilder t m@ means it should work identically on any supported platform.
class
  ( Monad m
  , Reflex t
  , Adjustable t m, NotReady t m
  , ViewSpace t (ViewBuilderSpace m)
  , ViewSpaceSupportsLayout t (ViewBuilderSpace m) layout
  , ViewLayout t layout
  ) => ViewBuilder t layout m | m -> t, m -> layout where
  -- |Create a static text view with the given configuration and place it in the hierarchy.
  buildTextView :: TextConfig t layout -> m (TextView t (ViewBuilderSpace m) layout)

  -- |Create a view containing some child hierarchy, returning the created view along with whatever the result of the inner build was.
  buildContainerView
    :: (ViewSpaceSupportsLayout t (ViewBuilderSpace m) layout')
    => ContainerConfig t layout layout'
    -> ViewBuilderForLayout layout' m a
    -> m (a, ContainerView t (ViewBuilderSpace m) layout layout')

  -- |Place a 'RawView' created externally in the view hierarchy being built, for example with functions or libraries that know the precise type of view
  -- hierarchy in use.
  --
  -- Behavior is undefined if the given view node is already in the view hierarchy somewhere else, though each specific view hierarchy has a defined behavior.
  placeRawView :: ContentLayout t layout -> RawView t (ViewBuilderSpace m) layout -> m ()

  -- |Wrap a 'RawView' for the appropriate 'ViewSpace' with Reflex functionality configured via the given 'RawViewConfig', such as the ability to change the
  -- view style or layout in response to @Event@s or recognize gestures using 'recognizeGesture'.
  --
  -- Behavior of a view wrapped twice will probably not be what you expect; updates associated with later invocations of @wrapRawView@ will probably stomp
  -- earlier invocations of @wrapRawView@, though it is undefined for any @ViewBuilder t m@ if that is so, and even if so which property updates will be
  -- applied.
  wrapRawView :: RawView t (ViewBuilderSpace m) layout -> RawViewConfig t -> m (View t (ViewBuilderSpace m) layout)

  -- |Given some gesture to recognize and any parameters of that recognition, return an @Event@ which fires each time the state of recognition of the gesture
  -- on the given view changes.
  --
  -- For example,
  --
  -- @
  --   do
  --     e <- recognizeGesture v GestureSpec_Pan
  --     _ <- buildTextView (defaultTextConfig { _textConfig_setText = show <$> e })
  -- @
  --
  -- Will show the state of any pan gesture occuring on @v@: @GestureState_None@ initially then @GestureState_Began …@ when the user starts dragging their
  -- finger across @v@, @GestureState_Changed …@ regularly while the user continues to slide their finger, and @GestureState_Ended …@ when the user lifts their
  -- finger.
  --
  -- __Warning:__ the returned @Event@ is only guaranteed to be valid in the current builder scope. It may (or may not) fire after the current scope is removed
  -- by way of 'Reflex.Class.Adjustable' methods such as 'Reflex.Class.runWithReplace'.
  recognizeGesture :: View t (ViewBuilderSpace m) layout -> GestureSpec gs -> m (Event t (GestureState (GestureData gs)))

  {-# INLINABLE buildTextView #-}
  default buildTextView
    :: (MonadTrans f, m ~ f n, ViewBuilderSpace n ~ ViewBuilderSpace m, ViewBuilder t layout n, Monad n)
    => TextConfig t layout -> m (TextView t (ViewBuilderSpace m) layout)
  buildTextView cfg = lift $ buildTextView cfg

  {-# INLINABLE placeRawView #-}
  default placeRawView
    :: (MonadTrans f, m ~ f n, ViewBuilderSpace n ~ ViewBuilderSpace m, ViewBuilder t layout n, Monad n)
    => ContentLayout t layout -> RawView t (ViewBuilderSpace m) layout -> m ()
  placeRawView l v = lift $ placeRawView l v

  {-# INLINABLE wrapRawView #-}
  default wrapRawView
    :: (MonadTrans f, m ~ f n, ViewBuilderSpace n ~ ViewBuilderSpace m, ViewBuilder t layout n, Monad n)
    => RawView t (ViewBuilderSpace m) layout -> RawViewConfig t -> m (View t (ViewBuilderSpace m) layout)
  wrapRawView v cfg = lift $ wrapRawView v cfg

  {-# INLINABLE recognizeGesture #-}
  default recognizeGesture
    :: (MonadTrans f, m ~ f n, ViewBuilderSpace n ~ ViewBuilderSpace m, ViewBuilder t layout n, Monad n)
    => View t (ViewBuilderSpace m) layout -> GestureSpec gs -> m (Event t (GestureState (GestureData gs)))
  recognizeGesture v spec = lift $ recognizeGesture v spec

-- |Pass through 'PostBuildT'.
instance (ViewBuilder t layout m, PerformEvent t m, MonadFix m, MonadHold t m) => ViewBuilder t layout (PostBuildT t m) where
  buildContainerView cfg (PostBuildT body) = PostBuildT $ buildContainerView cfg body

type instance ViewBuilderForLayout layout (PostBuildT t m) = PostBuildT t (ViewBuilderForLayout layout m)
type instance ViewBuilderSpace (PostBuildT t m) = ViewBuilderSpace m

-- |Pass through 'ReaderT'.
instance (ViewBuilder t layout m, Monad m) => ViewBuilder t layout (ReaderT r m) where
  buildContainerView cfg body = do
    r <- ask
    (a, vn) <- lift $ buildContainerView cfg (runReaderT body r)
    pure (a, vn)

type instance ViewBuilderForLayout layout (ReaderT r m) = ReaderT r (ViewBuilderForLayout layout m)
type instance ViewBuilderSpace (ReaderT r m) = ViewBuilderSpace m

-- |Pass through 'DynamicWriterT'.
instance (ViewBuilder t layout m, MonadHold t m, MonadFix m, Monoid w) => ViewBuilder t layout (DynamicWriterT t w m) where
  buildContainerView cfg (DynamicWriterT body) = DynamicWriterT $ do
    oldS <- get
    ((a, newS), vn) <- lift . buildContainerView cfg $ runStateT body oldS
    put newS
    pure (a, vn)

type instance ViewBuilderForLayout layout (DynamicWriterT t w m) = DynamicWriterT t w (ViewBuilderForLayout layout m)
type instance ViewBuilderSpace (DynamicWriterT t w m) = ViewBuilderSpace m

-- |Pass through 'RequesterT'.
instance (ViewBuilder t layout m, MonadHold t m, MonadFix m) => ViewBuilder t layout (RequesterT t request response m) where
  buildContainerView cfg (RequesterT body) = RequesterT $ do
    r <- ask
    oldS <- get
    ((a, newS), vn) <- lift . lift . buildContainerView cfg $ runReaderT (runStateT body oldS) r
    put newS
    pure (a, vn)

type instance ViewBuilderForLayout layout (RequesterT t request response m) = RequesterT t request response (ViewBuilderForLayout layout m)
type instance ViewBuilderSpace (RequesterT t request response m) = ViewBuilderSpace m

-- |Pass through 'EventWriterT'.
instance (ViewBuilder t layout m, MonadHold t m, MonadFix m, Semigroup w) => ViewBuilder t layout (EventWriterT t w m) where
  buildContainerView cfg (EventWriterT body) = EventWriterT $ do
    oldS <- get
    ((a, newS), vn) <- lift . buildContainerView cfg $ runStateT body oldS
    put newS
    pure (a, vn)

type instance ViewBuilderForLayout layout (EventWriterT t w m) = EventWriterT t w (ViewBuilderForLayout layout m)
type instance ViewBuilderSpace (EventWriterT t w m) = ViewBuilderSpace m

-- |Pass through 'QueryT'.
instance (ViewBuilder t layout m, MonadHold t m, MonadFix m, Group q, Query q, Additive q) => ViewBuilder t layout (QueryT t q m) where
  buildContainerView cfg (QueryT body) = QueryT $ do
    oldS <- get
    ((a, newS), vn) <- lift . buildContainerView cfg $ runStateT body oldS
    put newS
    pure (a, vn)

type instance ViewBuilderForLayout layout (QueryT t q m) = QueryT t q (ViewBuilderForLayout layout m)
type instance ViewBuilderSpace (QueryT t q m) = ViewBuilderSpace m
