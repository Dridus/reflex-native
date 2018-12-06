{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |Types used throughout "Reflex.Native.Test".
module Reflex.Native.Test.Types
  (
  -- * Unique identities
    TestIdentity, unTestIdentity, newTestIdentity, tshowTestIdentity
  -- * Test views
  , TestViewSpace, TestViewSpaceLayout(..), TestHolder, TestViewCommon(..), TestViewLayout(..)
  , TestLayoutHolder(..), newTestLayoutHolderConstraint, newTestLayoutHolderExplicit, newTestLayoutHolderFill, newTestLayoutHolderLinear
  , TestContainerViewLayout(..), TestContainerView(..), SomeTestContainerView(..)
  , TestTextView(..), SomeTestTextView(..)
  , TestMarker(..), SomeTestMarker(..)
  , TestView(..), testView_common, testView_identity, SomeTestView(..), SomeTestViews(..), withSomeTestViews
  -- ** Test views as diagnostic text
  , showsTestContainerView, showsTestView, showTestViewHierarchy, tshowTestContainerViewIdentity, tshowTestTextViewIdentity
  , tshowTestMarkerIdentity, tshowTestViewIdentity
  -- ** Traversing a test view hierarchy
  , traverseTestContainerView, traverseTestView
  -- * Test execution environment and evaluation monad
  , TestEnv(..), TestEvaluation(..)
  ) where

import Control.Concurrent.Chan (Chan)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Lens (Lens', Traversal', view)
import Control.Monad.Exception (MonadAsyncException, MonadException)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Strict (RWST)
import Data.Dependent.Sum (DSum)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Foldable (toList)
import Data.Functor.Identity (Identity(..))
import Data.Generics.Product (field)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import qualified Rank2
import Rank2 (apply)
import Reflex.Class (ffor)
import Reflex.Host.Class (ReflexHost(type EventHandle, type EventTrigger))
import Reflex.Native.Geometry (Axis(Horizontal, Vertical), KnownAxis(axisVal), Rect)
import Reflex.Native.TextStyle (TextStyle(..))
import Reflex.Native.ViewBuilder.Class
  ( ViewSpace(type ViewSpaceSupportsLayout, type RawContainerView, containerViewAsView, type RawTextView, textViewAsView, type RawView)
  , ContainerView(..), TextView(..), View(..)
  )
import Reflex.Native.ViewLayout.Class (ViewLayout(type ContainerLayout))
import Reflex.Native.ViewLayout.Constraint (ConstraintLayout, ViewConstraint)
import Reflex.Native.ViewLayout.Explicit (ExplicitLayout)
import Reflex.Native.ViewLayout.Fill (FillLayout)
import Reflex.Native.ViewLayout.Linear (LinearLayout, ContainerLayout(ContainerLayout_Linear, _containerLayout_linear_spacingBetween))
import Reflex.Native.ViewStyle (ViewStyle(..))
import Reflex.PerformEvent.Base (FireCommand)
import Reflex.Spider (SpiderHost, SpiderTimeline)
import Reflex.TriggerEvent.Base (EventTriggerRef, TriggerInvocation)
import System.IO.Unsafe (unsafePerformIO)


-- |Type of 'ViewSpace' for testing with the test types as the raw types, e.g. @RawView ~ TestView@.
data TestViewSpace

-- |Instance for testing.
instance ViewSpace t TestViewSpace where
  type ViewSpaceSupportsLayout t TestViewSpace = TestViewSpaceLayout t

  type RawContainerView t TestViewSpace layout layout' = TestContainerView t layout layout' TVar
  type RawTextView      t TestViewSpace layout         = TestTextView layout TVar
  type RawView          t TestViewSpace layout         = TestView t layout TVar

  containerViewAsView (ContainerView cv) = View (TestView_Container cv)
  textViewAsView (TextView tv) = View (TestView_Text tv)

class ViewLayout t layout => TestViewSpaceLayout t layout where
  makeTestContainerViewLayout :: MonadIO m => ContainerLayout t layout -> m (TestContainerViewLayout t layout TVar)

instance TestViewSpaceLayout t ConstraintLayout where
  makeTestContainerViewLayout _ = TestContainerViewLayout_Constraint <$> liftIO (newTVarIO mempty)

instance TestViewSpaceLayout t ExplicitLayout where
  makeTestContainerViewLayout _ = pure TestContainerViewLayout_Explicit

instance TestViewSpaceLayout t FillLayout where
  makeTestContainerViewLayout _ = pure TestContainerViewLayout_Fill

instance forall t axis. KnownAxis axis => TestViewSpaceLayout t (LinearLayout axis) where
  makeTestContainerViewLayout (ContainerLayout_Linear {..}) = pure $ TestContainerViewLayout_Linear (Proxy @axis) _containerLayout_linear_spacingBetween

-- |A unique identity for a test view, holder, marker, or similar thing qualified by what it's an identity for. Almost identical to "Data.Unique" except that
-- this has a more useful 'Show' instance for diagnostics.
newtype TestIdentity = TestIdentity { unTestIdentity :: Integer } deriving (Eq, Ord)

-- |Show a unique identifier for diagnostics.
tshowTestIdentity :: TestIdentity -> Text
tshowTestIdentity (TestIdentity i) = pack ('#' : show i)

-- |Shared reference to make unique 'TestIdentity' values
{-# NOINLINE nextTestIdentityRef #-}
nextTestIdentityRef :: IORef Integer
nextTestIdentityRef = unsafePerformIO $ newIORef 1

-- |Create a new 'TestIdentity' with a new serial number
newTestIdentity :: MonadIO m => m TestIdentity
newTestIdentity =
  fmap TestIdentity . liftIO . atomicModifyIORef' nextTestIdentityRef $ \ n ->
    let n' = succ n in (n', n')

-- |'ShowS' for a @'ViewStyle' Identity@ since 'Show' is needed for test assertion messages and so on.
showsViewStyle :: ViewStyle Identity -> ShowS
showsViewStyle (ViewStyle {..})
  = ('{':)
  . showString "bg=" . shows (runIdentity _viewStyle_backgroundColor)
  . ('}':)

-- |Parameters of a test view's layout within its container.
data TestViewLayout layout v where
  TestViewLayout_Constraint :: forall v. TestViewLayout ConstraintLayout v
  TestViewLayout_Explicit :: forall v. v Rect -> TestViewLayout ExplicitLayout v
  TestViewLayout_Fill :: forall v. TestViewLayout FillLayout v
  TestViewLayout_Linear :: forall (axis :: Axis) v. KnownAxis axis => Proxy axis -> TestViewLayout (LinearLayout axis) v

instance Show (TestViewLayout layout Identity) where
  showsPrec _ = \ case
    TestViewLayout_Constraint -> showString "constraint"
    TestViewLayout_Explicit (Identity r) -> showString "explicit " . shows r
    TestViewLayout_Fill -> showString "fill"
    TestViewLayout_Linear a -> case axisVal a of
      Horizontal -> showString "row"
      Vertical -> showString "column"

instance Rank2.Functor (TestViewLayout layout) where
  _ <$> TestViewLayout_Constraint = TestViewLayout_Constraint
  f <$> TestViewLayout_Explicit r = TestViewLayout_Explicit (f r)
  _ <$> TestViewLayout_Fill = TestViewLayout_Fill
  _ <$> TestViewLayout_Linear a = TestViewLayout_Linear a
instance Rank2.Apply (TestViewLayout layout) where
  TestViewLayout_Constraint <*> _                         = TestViewLayout_Constraint
  TestViewLayout_Explicit f <*> TestViewLayout_Explicit a = TestViewLayout_Explicit (apply f a)
  TestViewLayout_Fill <*> _                               = TestViewLayout_Fill
  TestViewLayout_Linear a <*> _                           = TestViewLayout_Linear a
instance Rank2.Foldable (TestViewLayout layout) where
  foldMap _ TestViewLayout_Constraint = mempty
  foldMap f (TestViewLayout_Explicit a) = f a
  foldMap _ TestViewLayout_Fill = mempty
  foldMap _ (TestViewLayout_Linear _) = mempty
instance Rank2.Traversable (TestViewLayout layout) where
  traverse _ TestViewLayout_Constraint   = pure TestViewLayout_Constraint
  traverse f (TestViewLayout_Explicit a) = TestViewLayout_Explicit <$> f a
  traverse _ TestViewLayout_Fill         = pure TestViewLayout_Fill
  traverse _ (TestViewLayout_Linear a)   = pure (TestViewLayout_Linear a)

-- |Common attributes of every view in a test view hierarchy. Parameterized by @v@ which wraps every value; @v ~ TVar@ during the building step, and
-- @f ~ Identity@ for frozen copies of the view hierarchy.
data TestViewCommon layout v = TestViewCommon
  { _testViewCommon_identity :: TestIdentity
  -- ^Unique identity of the view for distinguising it among others.
  , _testViewCommon_style :: ViewStyle v
  -- ^The style of the view.
  , _testViewCommon_accessibilityLabel :: v (Maybe Text)
  -- ^The accessibility label of the view.
  , _testViewCommon_layout :: TestViewLayout layout v
  } deriving (Generic)

-- |Show a @TestViewCommon@ for test assertion messages and the like. Usually used as the first part of showing the view type embedding the @TestViewCommon@.
instance Show (TestViewCommon layout Identity) where
  showsPrec _ (TestViewCommon {..})
    = ('#':) . shows (unTestIdentity _testViewCommon_identity)
    . showString " style=" . showsViewStyle _testViewCommon_style
    . showString " accessibilityLabel=" . shows (runIdentity _testViewCommon_accessibilityLabel)
    . showString " layout=" . shows _testViewCommon_layout

instance Show (TestViewCommon layout TVar) where
  showsPrec _ (TestViewCommon {..})
    = ('#':) . shows (unTestIdentity _testViewCommon_identity)

instance Rank2.Functor (TestViewCommon layout) where
  f <$> TestViewCommon a b c d = TestViewCommon a (f Rank2.<$> b) (f c) (f Rank2.<$> d)
instance Rank2.Apply (TestViewCommon layout) where
  TestViewCommon _ fb fc fd <*> TestViewCommon a b c d = TestViewCommon a (fb Rank2.<*> b) (apply fc c) (fd Rank2.<*> d)
instance Rank2.Foldable (TestViewCommon layout) where
  foldMap f (TestViewCommon _ b c d) = Rank2.foldMap f b <> f c <> Rank2.foldMap f d
instance Rank2.Traversable (TestViewCommon layout) where
  traverse f (TestViewCommon a b c d) = TestViewCommon a <$> Rank2.traverse f b <*> f c <*> Rank2.traverse f d

-- |Layout information for subviews of a 'TestContainerView'.
data TestContainerViewLayout t layout v where
  -- |Lay out the subviews according to a system of linear equations.
  TestContainerViewLayout_Constraint :: forall t v. v (IntMap (ViewConstraint t TestViewSpace)) -> TestContainerViewLayout t ConstraintLayout v
  -- |Lay out the subviews with fixed rectangles
  TestContainerViewLayout_Explicit :: forall t v. TestContainerViewLayout t ExplicitLayout v
  -- |Make all subviews the same size as the container.
  TestContainerViewLayout_Fill :: forall t v. TestContainerViewLayout t FillLayout v
  -- |Lay out the subviews in a horizontal or vertical line with some amount of space in between each.
  TestContainerViewLayout_Linear :: forall (axis :: Axis) t v. KnownAxis axis => Proxy axis -> Double -> TestContainerViewLayout t (LinearLayout axis) v

instance Show (TestContainerViewLayout t layout Identity) where
  showsPrec _ = \ case
    TestContainerViewLayout_Constraint (Identity cs) -> showString "constraints: " . showList (toList cs)
    TestContainerViewLayout_Explicit -> showString "explicit"
    TestContainerViewLayout_Fill -> showString "fill"
    TestContainerViewLayout_Linear a s ->
      let direction = case axisVal a of
            Horizontal -> showString "row with spacing "
            Vertical -> showString "column with spacing "
      in direction . shows s

instance Show (TestContainerViewLayout t layout TVar) where
  showsPrec _ = \ case
    TestContainerViewLayout_Constraint _ -> showString "constraints"
    TestContainerViewLayout_Explicit -> showString "explicit"
    TestContainerViewLayout_Fill -> showString "fill"
    TestContainerViewLayout_Linear a s ->
      let direction = case axisVal a of
            Horizontal -> showString "row with spacing "
            Vertical -> showString "column with spacing "
      in direction . shows s
  
instance Rank2.Functor (TestContainerViewLayout t layout) where
  f <$> TestContainerViewLayout_Constraint a = TestContainerViewLayout_Constraint (f a)
  _ <$> TestContainerViewLayout_Explicit = TestContainerViewLayout_Explicit
  _ <$> TestContainerViewLayout_Fill = TestContainerViewLayout_Fill
  _ <$> (TestContainerViewLayout_Linear a s) = TestContainerViewLayout_Linear a s
instance Rank2.Apply (TestContainerViewLayout layout t) where
  TestContainerViewLayout_Constraint fa <*> TestContainerViewLayout_Constraint a = TestContainerViewLayout_Constraint (apply fa a)
  TestContainerViewLayout_Explicit <*> _                                         = TestContainerViewLayout_Explicit
  TestContainerViewLayout_Fill <*> _                                             = TestContainerViewLayout_Fill
  TestContainerViewLayout_Linear a s <*> _                                       = TestContainerViewLayout_Linear a s
instance Rank2.Foldable (TestContainerViewLayout t layout) where
  foldMap f (TestContainerViewLayout_Constraint cs) = f cs
  foldMap _ TestContainerViewLayout_Explicit = mempty
  foldMap _ TestContainerViewLayout_Fill = mempty
  foldMap _ (TestContainerViewLayout_Linear _ _) = mempty
instance Rank2.Traversable (TestContainerViewLayout t layout) where
  traverse f (TestContainerViewLayout_Constraint cs) = TestContainerViewLayout_Constraint <$> f cs
  traverse _ TestContainerViewLayout_Explicit        = pure TestContainerViewLayout_Explicit
  traverse _ TestContainerViewLayout_Fill            = pure TestContainerViewLayout_Fill
  traverse _ (TestContainerViewLayout_Linear a s)    = pure (TestContainerViewLayout_Linear a s)

-- |A container view which has common view attributes and a collection of subviews.
data TestContainerView t layout layout' v = TestContainerView
  { _testContainerView_common :: TestViewCommon layout v
  -- ^The common view attributes for the container.
  , _testContainerView_layout :: TestContainerViewLayout t layout' v
  -- ^The layout information for the subviews of the container.
  , _testContainerView_contents :: v (Seq (TestView t layout' v))
  -- ^The subviews.
  } deriving (Generic)
-- can't instance rank-2 Functor on account of the fixed point - would need @v@ or @v'@ to be a Functor but they need to be natural.

-- |Show a 'TestContainerView' for test assertion messages and the like.
instance Show (TestContainerView t layout layout' Identity) where
  showsPrec _ = showsTestContainerView True

instance Show (TestContainerView t layout layout' TVar) where
  showsPrec _ (TestContainerView {..})
    = showString "container " . shows _testContainerView_common
    . showString " with layout " . shows _testContainerView_layout

-- |Show a 'TestContainerView' for test assertion messages and the like. Takes a boolean indicating whether subviews will be dumped (@True@) or not (@False@).
showsTestContainerView :: Bool -> TestContainerView t layout layout' Identity -> ShowS
showsTestContainerView recurse (TestContainerView {..})
  = showString "container " . shows _testContainerView_common
  . showString " with layout " . shows _testContainerView_layout
  . (if recurse then (' ':) . showList (toList _testContainerView_contents) else id)

-- |Traverse some effect through a @'TestContainerView' v@ while changing @v -> v'@. See 'traverseTestView' for how this is commonly used.
-- The traversal effect needs to accept an additional mapping effect to apply inside in order to handle the fixed point @_testContainerView_contents@.
traverseTestContainerView
  :: Applicative f
  => (forall a b. (a -> f b) -> v a -> f (v' b))
  -> TestContainerView t layout layout' v -> f (TestContainerView t layout layout' v')
traverseTestContainerView f (TestContainerView {..}) =
  TestContainerView
    <$> Rank2.traverse (f pure) _testContainerView_common
    <*> Rank2.traverse (f pure) _testContainerView_layout
    <*> f (traverse (traverseTestView f)) _testContainerView_contents

-- |Show the type and identity of a test container view, equivalent to @'tshowTestViewIdentity' . 'TestView_Container'@
tshowTestContainerViewIdentity :: TestContainerView t layout layout' v -> Text
tshowTestContainerViewIdentity = tshowTestViewIdentity . TestView_Container

data SomeTestContainerView t v = forall layout layout'. SomeTestContainerView (TestContainerView t layout layout' v)

instance Eq (SomeTestContainerView t v) where
  SomeTestContainerView cv1 == SomeTestContainerView cv2 =
    _testViewCommon_identity (_testContainerView_common cv1) == _testViewCommon_identity (_testContainerView_common cv2)
instance Show (SomeTestContainerView t Identity) where
  showsPrec p (SomeTestContainerView cv) = showsPrec p cv
instance Show (SomeTestContainerView t TVar) where
  showsPrec p (SomeTestContainerView cv) = showsPrec p cv

-- |A text display view which has common view attributes, a text style, and whatever the current/captured text is.
data TestTextView layout v = TestTextView
  { _testTextView_common :: TestViewCommon layout v
  -- ^The common view attributes for the text view.
  , _testTextView_style :: TextStyle v
  -- ^The style to display the text with.
  , _testTextView_text :: v Text
  -- ^The actual text.
  } deriving (Generic)

-- |'ShowS' for a @'TextStyle' Identity@ since 'Show' is needed for test assertion messages and so on.
showsTextStyle :: TextStyle Identity -> ShowS
showsTextStyle (TextStyle {..})
  = showString "{color=" . shows (runIdentity _textStyle_textColor)
  . showString " font=" . shows (runIdentity _textStyle_font)
  . ('}':)

-- |Show a 'TestTextView' for test assertion messages and the like.
instance Show (TestTextView layout Identity) where
  showsPrec _ (TestTextView {..})
    = showString "text " . shows _testTextView_common
    . showString " textStyle=" . showsTextStyle _testTextView_style
    . showString " text=" . shows (runIdentity _testTextView_text)

instance Show (TestTextView layout TVar) where
  showsPrec _ (TestTextView {..})
    = showString "text " . shows _testTextView_common

instance Rank2.Functor (TestTextView layout) where
  f <$> TestTextView a b c = TestTextView (f Rank2.<$> a) (f Rank2.<$> b) (f c)
instance Rank2.Apply (TestTextView layout) where
  TestTextView fa fb fc <*> TestTextView a b c = TestTextView (fa Rank2.<*> a) (fb Rank2.<*> b) (apply fc c)
instance Rank2.Foldable (TestTextView layout) where
  foldMap f (TestTextView a b c) = Rank2.foldMap f a <> Rank2.foldMap f b <> f c
instance Rank2.Traversable (TestTextView layout) where
  traverse f (TestTextView a b c) = TestTextView <$> Rank2.traverse f a <*> Rank2.traverse f b <*> f c

-- |Show the type and identity of a test text view, equivalent to @'tshowTestViewIdentity' . 'TestView_Text'@
tshowTestTextViewIdentity :: TestTextView layout v -> Text
tshowTestTextViewIdentity = tshowTestViewIdentity . TestView_Text

data SomeTestTextView v = forall layout. SomeTestTextView (TestTextView layout v)

instance Eq (SomeTestTextView v) where
  SomeTestTextView tv1 == SomeTestTextView tv2 =
    _testViewCommon_identity (_testTextView_common tv1) == _testViewCommon_identity (_testTextView_common tv2)

instance Show (SomeTestTextView Identity) where
  showsPrec p (SomeTestTextView tv) = showsPrec p tv

instance Show (SomeTestTextView TVar) where
  showsPrec p (SomeTestTextView tv) = showsPrec p tv

-- |A marker view node which doesn't have any display but denotes the boundary between replaceable view segments.
data TestMarker t layout = TestMarker
  { _testMarker_identity :: TestIdentity
  -- ^The unique identity of the marker.
  , _testMarker_parent :: TVar (Maybe (TestHolder t layout))
  -- ^Where the marker is installed, or Nothing if it's not installed.
  } deriving (Generic)

-- |Test for equal identity of two marker nodes
instance Eq (TestMarker t layout) where
  a == b = _testMarker_identity a == _testMarker_identity b

-- |Show a 'TestMarker' for test assertion messages and the like.
instance Show (TestMarker t layout) where
  showsPrec _ (TestMarker {..}) = showString "marker #" . shows (unTestIdentity _testMarker_identity)

-- |Show the type and identity of a test marker, equivalent to @'tshowTestViewIdentity' . 'TestView_Marker'@
tshowTestMarkerIdentity :: TestMarker t layout -> Text
tshowTestMarkerIdentity = tshowTestViewIdentity . TestView_Marker

data SomeTestMarker t = forall layout. SomeTestMarker (TestMarker t layout)

instance Eq (SomeTestMarker t) where
  SomeTestMarker m1 == SomeTestMarker m2 = _testMarker_identity m1 == _testMarker_identity m2

instance Show (SomeTestMarker t) where
  showsPrec p (SomeTestMarker m) = showsPrec p m

-- |A node in the view hierarchy, either one of the @Test*View@ types or a special marker used during build time to isolate sections of the subviews.
data TestView t layout v
  = forall layout'. TestView_Container (TestContainerView t layout layout' v)
  | TestView_Text (TestTextView layout v)
  | TestView_Marker (TestMarker t layout)

-- |Test for equal identity of two view nodes
instance Eq (TestView t layout v) where
  a == b = view testView_identity a == view testView_identity b

-- |Show a 'TestView' for test assertion messages and the like.
instance Show (TestView t layout Identity) where
  showsPrec _ = showsTestView True

instance Show (TestView t layout TVar) where
  showsPrec _ = \ case
    TestView_Container cv -> shows cv
    TestView_Text tv -> shows tv
    TestView_Marker m -> shows m

-- |Show a 'TestView' for test assertion messages and the like. Takes a boolean which controls whether subviews will be dumped (@True@) or not (@False).
showsTestView :: Bool -> TestView t layout Identity -> ShowS
showsTestView recurse = \ case
  -- each of the view types includes show output indicating their type, so don't duplicate it here
  TestView_Container cv -> showsTestContainerView recurse cv
  TestView_Text tv -> shows tv
  TestView_Marker m -> shows m

-- |Show a 'TestView' hierarchy on multiple lines with indenting.
showTestViewHierarchy :: forall layout t. String -> Seq (TestView t layout Identity) -> [String]
showTestViewHierarchy prefix = DList.toList . go prefix
  where
    go :: forall layout'. String -> Seq (TestView t layout' Identity) -> DList String
    go indent = foldMap (visit indent) . toList

    visit :: forall layout'. String -> TestView t layout' Identity -> DList String
    visit indent = \ case
      TestView_Container cv ->
        DList.cons
          (indent ++ showsTestContainerView False cv "")
          (go (' ':' ':indent) . runIdentity . _testContainerView_contents $ cv)
      other -> DList.singleton $ indent ++ showsTestView False other ""

-- |Traverse some effect through a @'TestView' v@ while changing @v -> v'@. This is used to do any recursive effect on a view hierarchy, such as freezing a
-- @'TestView' TVar@ into a @'TestView' Identity@ via @atomically . traverseTestView (\ f -> pure . Identity <=< f <=< readTVar)@.
-- The traversal effect needs to accept an additional mapping effect to apply inside in order to handle the fixed point @_testContainerView_contents@ if the
-- view is a @TestView_Container@.
traverseTestView
  :: Applicative f
  => (forall a b. (a -> f b) -> v a -> f (v' b))
  -> TestView t layout v -> f (TestView t layout v')
traverseTestView f = \ case
  TestView_Container cv -> TestView_Container <$> traverseTestContainerView f cv
  TestView_Text tv -> TestView_Text <$> Rank2.traverse (f pure) tv
  TestView_Marker m -> pure (TestView_Marker m)

-- |Show the type and identity of a view node
tshowTestViewIdentity :: TestView t layout v -> Text
tshowTestViewIdentity = \ case
  TestView_Container cv -> "container " <> tshowTestIdentity (_testViewCommon_identity . _testContainerView_common $ cv)
  TestView_Text tv -> "text " <> tshowTestIdentity (_testViewCommon_identity . _testTextView_common $ tv)
  TestView_Marker m -> "marker " <> tshowTestIdentity (_testMarker_identity m)

-- |Traverse to the 'TestViewCommon' in a 'TestView', if it's not a 'TestView_Marker'.
testView_common :: Traversal' (TestView t layout v) (TestViewCommon layout v)
testView_common f = \ case
  TestView_Container cv -> TestView_Container <$> field @"_testContainerView_common" f cv
  TestView_Text tv -> TestView_Text <$> field @"_testTextView_common" f tv
  other -> pure other

-- |Traverse to the unique identity in a 'TestView'.
testView_identity :: forall t layout v. Lens' (TestView t layout v) TestIdentity
testView_identity f = \ case
  TestView_Container (cv@(TestContainerView { _testContainerView_common }) :: TestContainerView t layout layout' v) ->
    ffor (field @"_testViewCommon_identity" f _testContainerView_common) $ \ common' ->
      TestView_Container cv { _testContainerView_common = common' }
  TestView_Text tv -> TestView_Text <$> (field @"_testTextView_common" . field @"_testViewCommon_identity") f tv
  TestView_Marker (TestMarker i p) -> fmap (\ i' -> TestView_Marker $ TestMarker i' p) (f i)

data SomeTestView t v = forall layout. SomeTestView (TestView t layout v)

instance Eq (SomeTestView t v) where
  SomeTestView v1 == SomeTestView v2 = view testView_identity v1 == view testView_identity v2

instance Show (SomeTestView t Identity) where
  showsPrec p (SomeTestView v) = showsPrec p v

instance Show (SomeTestView t TVar) where
  showsPrec p (SomeTestView v) = showsPrec p v

data SomeTestViews t v = forall layout. SomeTestViews (Seq (TestView t layout v))

withSomeTestViews :: SomeTestViews t v -> (forall layout. Seq (TestView t layout v) -> a) -> a
withSomeTestViews (SomeTestViews vs) f = f vs

-- |Type which holds a sequence of views. The same type as @_testContainerView_contents@ for @'TestContainerView' TVar@
type TestHolder t layout = TVar (Seq (TestView t layout TVar))

data TestLayoutHolder t layout where
  TestLayoutHolder_Constraint ::
    { _testLayoutHolder_constraint_constraints :: TVar (IntMap (ViewConstraint t TestViewSpace))
    , _testLayoutHolder_constraint_nextConstraintId :: TVar Int
    } -> TestLayoutHolder t ConstraintLayout
  TestLayoutHolder_Explicit :: TestLayoutHolder t ExplicitLayout
  TestLayoutHolder_Fill :: TestLayoutHolder t FillLayout
  TestLayoutHolder_Linear :: TestLayoutHolder t (LinearLayout axis)

newTestLayoutHolderConstraint :: IO (TestLayoutHolder t ConstraintLayout)
newTestLayoutHolderConstraint =
  TestLayoutHolder_Constraint
    <$> newTVarIO IntMap.empty
    <*> newTVarIO 0

newTestLayoutHolderExplicit :: IO (TestLayoutHolder t ExplicitLayout)
newTestLayoutHolderExplicit =
  pure TestLayoutHolder_Explicit
  
newTestLayoutHolderFill :: IO (TestLayoutHolder t FillLayout)
newTestLayoutHolderFill =
  pure TestLayoutHolder_Fill
  
newTestLayoutHolderLinear :: IO (TestLayoutHolder t (LinearLayout axis))
newTestLayoutHolderLinear =
  pure TestLayoutHolder_Linear

-- |The environment of an in-progress test with a handle to the view hierarchy and the event processing channel.
data TestEnv x = TestEnv
  { _testEnv_rootHolder :: TestHolder (SpiderTimeline x) FillLayout
  -- ^The root of the view hierarchy.
  , _testEnv_rootLayout :: TestLayoutHolder (SpiderTimeline x) FillLayout
  -- ^The root layout context for the view hierarchy.
  , _testEnv_rootReady :: TVar Bool
  -- ^True iff the first build was immediately ready or it's been committed since.
  , _testEnv_eventChan :: Chan [DSum (EventTriggerRef (SpiderTimeline x)) TriggerInvocation]
  -- ^The event channel to write new event trigger invocations to.
  , _testEnv_fireCommand :: FireCommand (SpiderTimeline x) (SpiderHost x)
  -- ^The 'FireCommand' which is used to process events with the underlying host and then perform any actions triggered by those events.
  , _testEnv_stepCompleteEventHandle :: EventHandle (SpiderTimeline x) ()
  -- ^The event which is fired after each test evaluation step to ensure that event processing has been finished. This is especially required since @Chan@s can
  -- only be read by blocking, so we need an event to explicitly bookend the step.
  , _testEnv_stepCompleteTriggerRef :: IORef (Maybe (EventTrigger (SpiderTimeline x) ()))
  -- ^The trigger for @_testEnv_stepCompleteEvent@.
  }

-- |The monad for evaluating an in-progress test after the build has completed and has access to the state of the view hierarchy and event processing channel.
newtype TestEvaluation x layout a = TestEvaluation { unTestEvaluation :: RWST (TestEnv x) () (Seq (TestView (SpiderTimeline x) layout Identity)) (SpiderHost x) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException)
