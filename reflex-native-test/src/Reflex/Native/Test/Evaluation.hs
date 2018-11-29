{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- |Functions for evaluating the current state of a built UI hierarchy and making assertions about that state.
module Reflex.Native.Test.Evaluation
  (
  -- * Basic functions
    askRootViews, askRootReady, runFrame, selectFromViews, lookupFromViews
  -- * "Test.Hspec.Expectations" lifted to 'TestEvaluation'
  , expectationFailure, shouldBe, shouldSatisfy, shouldStartWith, shouldEndWith, shouldContain, shouldMatchList, shouldReturn, shouldNotBe, shouldNotSatisfy
  , shouldNotContain, shouldNotReturn
  -- * Expectations similar to @Test.Hspec.Expectations.Lens@ for views
  , shouldHave, shouldNotHave, shouldView, shouldPreview, shouldList
  ) where

import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Lens (Getting, has, hasn't, preview, toListOf, view)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS.Strict (asks, get, gets)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import Data.Monoid (All, Any, Endo, First)
import Data.Sequence (Seq)
import GHC.Stack (HasCallStack)
import Reflex.Host.Class (ReflexHost(type HostFrame), runHostFrame)
import Reflex.Native.Test.Types (TestEnv(..), TestEvaluation(..), TestView, showTestViewHierarchy)
import Reflex.Spider.Internal (HasSpiderTimeline, SpiderTimeline)
import Reflex.TriggerEvent.Base (TriggerEventT, runTriggerEventT)
import qualified Test.Hspec.Expectations as Hspec


-- |Get the current view hierarchy, updated each time 'processEventsAndRead' is run.
askRootViews :: TestEvaluation x layout (Seq (TestView (SpiderTimeline x) layout Identity))
askRootViews = TestEvaluation get

-- |Get whether the root has become ready.
askRootReady :: TestEvaluation x layout Bool
askRootReady = TestEvaluation (asks _testEnv_rootReady) >>= liftIO . readTVarIO

-- |Run a frame of Reflex, letting you sample, hold, subscribe to events, or create triggers during a 'TestEvaluation'.
runFrame
  :: forall x layout a. HasSpiderTimeline x
  => (TriggerEventT (SpiderTimeline x) (HostFrame (SpiderTimeline x)) a)
  -> TestEvaluation x layout a
runFrame action = TestEvaluation $ do
  chan <- asks _testEnv_eventChan
  lift $ runHostFrame (runTriggerEventT action chan)

-- |Apply some @Fold@ to the current view hierarchy returning the results.
selectFromViews :: Getting (Endo [a]) (Seq (TestView (SpiderTimeline x) layout Identity)) a -> TestEvaluation x layout [a]
selectFromViews f = TestEvaluation $ gets (toListOf f)

-- |Apply some @Fold@ to the current view hierarchy returning the first result if any.
lookupFromViews :: Getting (First a) (Seq (TestView (SpiderTimeline x) layout Identity)) a -> TestEvaluation x layout (Maybe a)
lookupFromViews f = TestEvaluation $ gets (preview f)

-- |Signal that an expectation failed with some message, aborting the test.
expectationFailure :: HasCallStack => String -> TestEvaluation x layout ()
expectationFailure = liftIO . Hspec.expectationFailure

infix 2 `shouldBe`, `shouldSatisfy`, `shouldStartWith`, `shouldEndWith`, `shouldContain`, `shouldMatchList`, `shouldReturn`
infix 2 `shouldNotBe`, `shouldNotSatisfy`, `shouldNotContain`, `shouldNotReturn`

-- |@actual \`shouldBe\` expected@ sets the expectation that @actual@ is equal to @expected@.
shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> TestEvaluation x layout ()
actual `shouldBe` expected = liftIO $ actual `Hspec.shouldBe` expected

-- |@v \`shouldSatisfy\` p@ sets the expectation that @p v@ is @True@.
shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> TestEvaluation x layout ()
v `shouldSatisfy` p = liftIO $ v `Hspec.shouldSatisfy` p

-- |@list \`shouldStartWith\` prefix@ sets the expectation that @list@ starts with @prefix@,
shouldStartWith :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> TestEvaluation x layout ()
xs `shouldStartWith` ys = liftIO $ xs `Hspec.shouldStartWith` ys

-- |@list \`shouldEndWith\` suffix@ sets the expectation that @list@ ends with @suffix@,
shouldEndWith :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> TestEvaluation x layout ()
xs `shouldEndWith` ys = liftIO $ xs `Hspec.shouldEndWith` ys

-- |@list \`shouldContain\` sublist@ sets the expectation that @sublist@ is contained, wholly and intact, anywhere in @list@.
shouldContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> TestEvaluation x layout ()
xs `shouldContain` ys = liftIO $ xs `Hspec.shouldContain` ys

-- |@xs \`shouldMatchList\` ys@ sets the expectation that @xs@ has the same elements that @ys@ has, possibly in another order
shouldMatchList :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> TestEvaluation x layout ()
xs `shouldMatchList` ys = liftIO $ xs `Hspec.shouldMatchList` ys

-- |@action \`shouldReturn\` expected@ sets the expectation that @action@ returns @expected@.
shouldReturn :: (HasCallStack, Show a, Eq a) => TestEvaluation x layout a -> a -> TestEvaluation x layout ()
action `shouldReturn` expected = action >>= (`shouldBe` expected)

-- |@actual \`shouldNotBe\` notExpected@ sets the expectation that @actual@ is not equal to @notExpected@
shouldNotBe :: (HasCallStack, Show a, Eq a) => a -> a -> TestEvaluation x layout ()
actual `shouldNotBe` notExpected = liftIO $ actual `Hspec.shouldNotBe` notExpected

-- |@v \`shouldNotSatisfy\` p@ sets the expectation that @p v@ is @False@.
shouldNotSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> TestEvaluation x layout ()
v `shouldNotSatisfy` p = liftIO $ v `Hspec.shouldNotSatisfy` p

-- |@list \`shouldNotContain\` sublist@ sets the expectation that @sublist@ is not contained anywhere in @list@.
shouldNotContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> TestEvaluation x layout ()
list `shouldNotContain` sublist = liftIO $ list `Hspec.shouldNotContain` sublist

-- |@action \`shouldNotReturn\` notExpected@ sets the expectation that @action@ does not return @notExpected@.
shouldNotReturn :: (HasCallStack, Show a, Eq a) => TestEvaluation x layout a -> a -> TestEvaluation x layout ()
action `shouldNotReturn` notExpected = action >>= (`shouldNotBe` notExpected)

infixl 2 `shouldHave`, `shouldNotHave`, `shouldView`, `shouldPreview`, `shouldList`

-- | @s \`shouldHave\` l@ sets the expectation that 'Fold' @l@ has non-zero number of targets in the view hierarchy
--
-- > s `shouldBe` t ≡ s `shouldHave` only t
--
-- @
-- shouldHave :: 'Getter'     (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldHave :: 'Fold'       (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldHave :: 'Iso''       (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldHave :: 'Lens''      (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldHave :: 'Traversal'' (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldHave :: 'Prism''     (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- @
shouldHave :: HasCallStack => Getting Any (Seq (TestView (SpiderTimeline x) layout Identity)) a -> TestEvaluation x layout ()
shouldHave l = do
  vs <- askRootViews
  unless (has l vs) $
    expectationFailure $ "Fold had zero targets but expected at least one in:\n" ++ intercalate "\n" (showTestViewHierarchy "  " vs)

-- | @shouldNotHave l@ sets the expectation that 'Fold' @l@ has exactly zero targets in the view hierarchy
--
-- @
-- shouldNotHave :: 'Getter'     (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldNotHave :: 'Fold'       (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldNotHave :: 'Iso''       (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldNotHave :: 'Lens''      (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldNotHave :: 'Traversal'' (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldNotHave :: 'Prism''     (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- @
shouldNotHave :: (HasCallStack, Show a) => Getting All (Seq (TestView (SpiderTimeline x) layout Identity)) a -> TestEvaluation x layout ()
shouldNotHave l = do
  vs <- askRootViews
  unless (hasn't l vs) $ do
    expectationFailure $ "Fold was supposed to have zero targets in:\n" ++ intercalate "\n" (showTestViewHierarchy "  " vs)

-- | @l \`shouldView\` t@ sets the expectation that you can see target @t@ in the view hierarchy though a 'Getter' @l@
--
-- @
-- shouldView ::           ('Show' a, 'Eq' a) => 'Getter'     (Seq ('TestView' (SpiderTimeline x) Identity)) a -> a -> 'TestEvaluation' x layout ()
-- shouldView :: ('Data.Monoid.Monoid' m, 'Show' a, 'Eq' a) => 'Fold'       (Seq ('TestView' (SpiderTimeline x) Identity)) m -> a -> 'TestEvaluation' x layout ()
-- shouldView ::           ('Show' a, 'Eq' a) => 'Iso''       (Seq ('TestView' (SpiderTimeline x) Identity)) a -> a -> 'TestEvaluation' x layout ()
-- shouldView ::           ('Show' a, 'Eq' a) => 'Lens''      (Seq ('TestView' (SpiderTimeline x) Identity)) a -> a -> 'TestEvaluation' x layout ()
-- shouldView :: ('Data.Monoid.Monoid' m, 'Show' a, 'Eq' a) => 'Traversal'' (Seq ('TestView' (SpiderTimeline x) Identity)) m -> a -> 'TestEvaluation' x layout ()
-- shouldView :: ('Data.Monoid.Monoid' m, 'Show' a, 'Eq' a) => 'Prism''     (Seq ('TestView' (SpiderTimeline x) Identity)) m -> a -> 'TestEvaluation' x layout ()
-- @
shouldView :: (HasCallStack, Show a, Eq a) => Getting a (Seq (TestView (SpiderTimeline x) layout Identity)) a -> a -> TestEvaluation x layout ()
l `shouldView` t = do
  vs <- askRootViews
  let t' = view l vs
  when (t /= t') $
    expectationFailure $ "expected " ++ show t ++ " but got " ++ show t' ++ " in:\n" ++ intercalate "\n" (showTestViewHierarchy "  " vs)

-- | @l \`shouldPreview\` t@ sets the expectation that your @y@ is the first target of the 'Fold' @l@ in the view hierarchy
--
-- @
-- shouldPreview :: ('Show' a, 'Eq' a) => 'Getter'     (Seq ('TestView' (SpiderTimeline x) Identity)) a -> a -> 'TestEvaluation' x layout ()
-- shouldPreview :: ('Show' a, 'Eq' a) => 'Fold'       (Seq ('TestView' (SpiderTimeline x) Identity)) a -> a -> 'TestEvaluation' x layout ()
-- shouldPreview :: ('Show' a, 'Eq' a) => 'Lens''      (Seq ('TestView' (SpiderTimeline x) Identity)) a -> a -> 'TestEvaluation' x layout ()
-- shouldPreview :: ('Show' a, 'Eq' a) => 'Iso''       (Seq ('TestView' (SpiderTimeline x) Identity)) a -> a -> 'TestEvaluation' x layout ()
-- shouldPreview :: ('Show' a, 'Eq' a) => 'Traversal'' (Seq ('TestView' (SpiderTimeline x) Identity)) a -> a -> 'TestEvaluation' x layout ()
-- shouldPreview :: ('Show' a, 'Eq' a) => 'Prism''     (Seq ('TestView' (SpiderTimeline x) Identity)) a -> a -> 'TestEvaluation' x layout ()
-- @
shouldPreview :: (HasCallStack, Show a, Eq a) => Getting (First a) (Seq (TestView (SpiderTimeline x) layout Identity)) a -> a -> TestEvaluation x layout ()
l `shouldPreview` t = do
  vs <- askRootViews
  let t'May = preview l vs
  when (Just t /= t'May) $
    expectationFailure $ "expected (Just) " ++ show t ++ " but got " ++ show t'May ++ " in:\n" ++ intercalate "\n" (showTestViewHierarchy "  " vs)

-- | @l \`shouldList\` ts@ sets the expectation that @ts@ is a list of the Fold @l@ targets in the view hierarchy.
--
-- @
-- shouldList :: ('Show' a, 'Eq' a) => s -> [a] -> 'Getter'     (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldList :: ('Show' a, 'Eq' a) => s -> [a] -> 'Fold'       (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldList :: ('Show' a, 'Eq' a) => s -> [a] -> 'Lens''      (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldList :: ('Show' a, 'Eq' a) => s -> [a] -> 'Iso''       (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldList :: ('Show' a, 'Eq' a) => s -> [a] -> 'Traversal'' (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- shouldList :: ('Show' a, 'Eq' a) => s -> [a] -> 'Prism''     (Seq ('TestView' (SpiderTimeline x) Identity)) a -> 'TestEvaluation' x layout ()
-- @
shouldList :: (HasCallStack, Show a, Eq a) => Getting (Endo [a]) (Seq (TestView (SpiderTimeline x) layout Identity)) a -> [a] -> TestEvaluation x layout ()
l `shouldList` ts = do
  vs <- askRootViews
  let ts' = toListOf l vs
  when (ts /= ts') $
    expectationFailure $ "expected " ++ show ts ++ " but got " ++ show ts' ++ " in:\n" ++ intercalate "\n" (showTestViewHierarchy "  " vs)
