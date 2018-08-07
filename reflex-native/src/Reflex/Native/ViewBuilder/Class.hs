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
import Reflex.Native.Gesture (GestureData, GestureSpec, GestureState)
import Reflex.Native.TextConfig (TextConfig)
import Reflex.Native.ViewConfig (RawViewConfig)
import Reflex.Native.ViewLayout.Class (ViewLayout(type ContentLayout))
import Reflex.Native.ViewLayout.Explicit (ExplicitLayout)
import Reflex.Native.ViewLayout.Fill (FillLayout)
import Reflex.Native.ViewLayout.Linear (ColumnLayout, RowLayout)
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
class ViewSpace space where
  -- |The type of container views in the underlying view system, e.g. @UIView@ on UIKit or @ViewGroup@ on Android.
  type RawContainerView space layout layout' t :: *

  containerViewAsView :: ContainerView space layout layout' t -> View space layout t

  -- |The type of text views in the underlying view system, e.g. @UILabel@ on UIKit or @TextView@ on Android.
  type RawTextView space layout t :: *

  textViewAsView :: TextView space layout t -> View space layout t

  -- |The type of any arbitrary view in the underlying view system that can be installed via 'placeRawView' or installed and made Reflex-aware using
  -- 'wrapRawView', e.g. @UIView@ on UIKit or @View@ on Android.
  type RawView space layout t :: *

-- |Wrapper around a 'RawContainerView' for a given 'ViewSpace'.
newtype ContainerView space layout layout' t = ContainerView { _containerView_raw :: RawContainerView space layout layout' t }
deriving instance Eq (RawContainerView space layout layout' t) => Eq (ContainerView space layout layout' t)
deriving instance Show (RawContainerView space layout layout' t) => Show (ContainerView space layout layout' t)

-- |Wrapper around a 'RawTextView' for a given 'ViewSpace'.
newtype TextView space layout t = TextView { _textView_raw :: RawTextView space layout t }
deriving instance Eq (RawTextView space layout t) => Eq (TextView space layout t)
deriving instance Show (RawTextView space layout t) => Show (TextView space layout t)

-- |Wrapper around a 'RawView' for a given 'ViewSpace'.
newtype View space layout t = View { _view_raw :: RawView space layout t }
deriving instance Eq (RawView space layout t) => Eq (View space layout t)
deriving instance Show (RawView space layout t) => Show (View space layout t)

-- |Typeclass for monads used to build view hierarchies which react over time to events in a cross-platform way. A function being polymorphic over
-- @ViewBuilder t m@ means it should work identically on any supported platform.
class
  ( Monad m
  , Reflex t
  , Adjustable t m, NotReady t m
  , ViewSpace (ViewBuilderSpace m)
  , ViewLayout (ViewBuilderLayout m) t
  , ViewLayoutSupport ExplicitLayout m
  , ViewLayoutSupport FillLayout m
  , ViewLayoutSupport ColumnLayout m
  , ViewLayoutSupport RowLayout m
  ) => ViewBuilder t m | m -> t where
  -- |The type of layout for subviews of the current builder monad.
  type ViewBuilderLayout m :: *

  -- |The type of a similar builder to @m@ except using a different layout type.
  -- It should hold that @ViewBuilderLayout (ViewBuilderForLayout layout m) ~ layout@ (that is, that changing the layout type is natural)
  -- and @ViewBuilderForLayout (ViewBuilderLayout m) m ~ m@ (that is, that changing the layout type doesn't affect @m@ other than the layout type).
  type ViewBuilderForLayout layout m :: * -> *

  -- |The associated 'ViewSpace' for this builder monad.
  type ViewBuilderSpace m :: *

  -- |The constraint required for supporting a particular layout type in this builder monad.
  type ViewLayoutSupport layout m :: Constraint

  -- |Create a static text view with the given configuration and place it in the hierarchy.
  buildTextView :: TextConfig (ViewBuilderLayout m) t -> m (TextView (ViewBuilderSpace m) (ViewBuilderLayout m) t)

  -- |Create a view containing some child hierarchy, returning the created view along with whatever the result of the inner build was.
  buildContainerView
    :: (n ~ ViewBuilderForLayout layout' m, ViewLayoutSupport layout' n, ViewLayout layout' t)
    => ContainerConfig (ViewBuilderLayout m) layout' t
    -> n a
    -> m (a, ContainerView (ViewBuilderSpace m) (ViewBuilderLayout m) layout' t)

  -- |Place a 'RawView' created externally in the view hierarchy being built, for example with functions or libraries that know the precise type of view
  -- hierarchy in use.
  --
  -- Behavior is undefined if the given view node is already in the view hierarchy somewhere else, though each specific view hierarchy has a defined behavior.
  placeRawView :: ContentLayout (ViewBuilderLayout m) t -> RawView (ViewBuilderSpace m) (ViewBuilderLayout m) t -> m ()

  -- |Wrap a 'RawView' for the appropriate 'ViewSpace' with Reflex functionality configured via the given 'RawViewConfig', such as the ability to change the
  -- view style or layout in response to @Event@s or recognize gestures using 'recognizeGesture'.
  --
  -- Behavior of a view wrapped twice will probably not be what you expect; updates associated with later invocations of @wrapRawView@ will probably stomp
  -- earlier invocations of @wrapRawView@, though it is undefined for any @ViewBuilder t m@ if that is so, and even if so which property updates will be
  -- applied.
  wrapRawView :: RawView (ViewBuilderSpace m) layout t -> RawViewConfig t -> m (View (ViewBuilderSpace m) layout t)

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
  recognizeGesture :: View (ViewBuilderSpace m) layout t -> GestureSpec gs -> m (Event t (GestureState (GestureData gs)))

  {-# INLINABLE buildTextView #-}
  default buildTextView
    :: (MonadTrans f, m ~ f n, ViewBuilderLayout n ~ ViewBuilderLayout m, ViewBuilderSpace n ~ ViewBuilderSpace m, ViewBuilder t n, Monad n)
    => TextConfig (ViewBuilderLayout m) t -> m (TextView (ViewBuilderSpace m) (ViewBuilderLayout m) t)
  buildTextView cfg = lift $ buildTextView cfg

  {-# INLINABLE placeRawView #-}
  default placeRawView
    :: (MonadTrans f, m ~ f n, ViewBuilderLayout n ~ ViewBuilderLayout m, ViewBuilderSpace n ~ ViewBuilderSpace m, ViewBuilder t n, Monad n)
    => ContentLayout (ViewBuilderLayout m) t -> RawView (ViewBuilderSpace m) (ViewBuilderLayout m) t -> m ()
  placeRawView l v = lift $ placeRawView l v

  {-# INLINABLE wrapRawView #-}
  default wrapRawView
    :: (MonadTrans f, m ~ f n, ViewBuilderLayout n ~ ViewBuilderLayout m, ViewBuilderSpace n ~ ViewBuilderSpace m, ViewBuilder t n, Monad n)
    => RawView (ViewBuilderSpace m) layout t -> RawViewConfig t -> m (View (ViewBuilderSpace m) layout t)
  wrapRawView v cfg = lift $ wrapRawView v cfg

  {-# INLINABLE recognizeGesture #-}
  default recognizeGesture
    :: (MonadTrans f, m ~ f n, ViewBuilderLayout n ~ ViewBuilderLayout m, ViewBuilderSpace n ~ ViewBuilderSpace m, ViewBuilder t n, Monad n)
    => View (ViewBuilderSpace m) layout t -> GestureSpec gs -> m (Event t (GestureState (GestureData gs)))
  recognizeGesture v spec = lift $ recognizeGesture v spec

-- |Pass through 'PostBuildT'.
instance (ViewBuilder t m, PerformEvent t m, MonadFix m, MonadHold t m) => ViewBuilder t (PostBuildT t m) where
  type ViewBuilderLayout (PostBuildT t m) = ViewBuilderLayout m
  type ViewBuilderForLayout layout (PostBuildT t m) = PostBuildT t (ViewBuilderForLayout layout m)
  type ViewBuilderSpace (PostBuildT t m) = ViewBuilderSpace m
  type ViewLayoutSupport layout (PostBuildT t m) = ViewLayoutSupport layout m
  buildContainerView cfg (PostBuildT body) = PostBuildT $ buildContainerView cfg body

-- |Pass through 'ReaderT'.
instance (ViewBuilder t m, Monad m) => ViewBuilder t (ReaderT r m) where
  type ViewBuilderLayout (ReaderT r m) = ViewBuilderLayout m
  type ViewBuilderForLayout layout (ReaderT r m) = ReaderT r (ViewBuilderForLayout layout m)
  type ViewBuilderSpace (ReaderT r m) = ViewBuilderSpace m
  type ViewLayoutSupport layout (ReaderT r m) = ViewLayoutSupport layout m
  buildContainerView cfg body = do
    r <- ask
    (a, vn) <- lift $ buildContainerView cfg (runReaderT body r)
    pure (a, vn)

-- |Pass through 'DynamicWriterT'.
instance (ViewBuilder t m, MonadHold t m, MonadFix m, Monoid w) => ViewBuilder t (DynamicWriterT t w m) where
  type ViewBuilderLayout (DynamicWriterT t w m) = ViewBuilderLayout m
  type ViewBuilderForLayout layout (DynamicWriterT t w m) = DynamicWriterT t w (ViewBuilderForLayout layout m)
  type ViewBuilderSpace (DynamicWriterT t w m) = ViewBuilderSpace m
  type ViewLayoutSupport layout (DynamicWriterT t w m) = ViewLayoutSupport layout m
  buildContainerView cfg (DynamicWriterT body) = DynamicWriterT $ do
    oldS <- get
    ((a, newS), vn) <- lift . buildContainerView cfg $ runStateT body oldS
    put newS
    pure (a, vn)

-- |Pass through 'RequesterT'.
instance (ViewBuilder t m, MonadHold t m, MonadFix m) => ViewBuilder t (RequesterT t request response m) where
  type ViewBuilderLayout (RequesterT t request response m) = ViewBuilderLayout m
  type ViewBuilderForLayout layout (RequesterT t request response m) = RequesterT t request response (ViewBuilderForLayout layout m)
  type ViewBuilderSpace (RequesterT t request response m) = ViewBuilderSpace m
  type ViewLayoutSupport layout (RequesterT t request response m) = ViewLayoutSupport layout m
  buildContainerView cfg (RequesterT body) = RequesterT $ do
    r <- ask
    oldS <- get
    ((a, newS), vn) <- lift . lift . buildContainerView cfg $ runReaderT (runStateT body oldS) r
    put newS
    pure (a, vn)

-- |Pass through 'EventWriterT'.
instance (ViewBuilder t m, MonadHold t m, MonadFix m, Semigroup w) => ViewBuilder t (EventWriterT t w m) where
  type ViewBuilderLayout (EventWriterT t w m) = ViewBuilderLayout m
  type ViewBuilderForLayout layout (EventWriterT t w m) = EventWriterT t w (ViewBuilderForLayout layout m)
  type ViewBuilderSpace (EventWriterT t w m) = ViewBuilderSpace m
  type ViewLayoutSupport layout (EventWriterT t w m) = ViewLayoutSupport layout m
  buildContainerView cfg (EventWriterT body) = EventWriterT $ do
    oldS <- get
    ((a, newS), vn) <- lift . buildContainerView cfg $ runStateT body oldS
    put newS
    pure (a, vn)

-- |Pass through 'QueryT'.
instance (ViewBuilder t m, MonadHold t m, MonadFix m, Group q, Query q, Additive q) => ViewBuilder t (QueryT t q m) where
  type ViewBuilderLayout (QueryT t q m) = ViewBuilderLayout m
  type ViewBuilderForLayout layout (QueryT t q m) = QueryT t q (ViewBuilderForLayout layout m)
  type ViewBuilderSpace (QueryT t q m) = ViewBuilderSpace m
  type ViewLayoutSupport layout (QueryT t q m) = ViewLayoutSupport layout m
  buildContainerView cfg (QueryT body) = QueryT $ do
    oldS <- get
    ((a, newS), vn) <- lift . buildContainerView cfg $ runStateT body oldS
    put newS
    pure (a, vn)

