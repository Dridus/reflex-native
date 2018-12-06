{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Reflex.Native.Examples.Draggy where

import Data.AdditiveGroup ((^+^), zeroV)
import Data.Default (def)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Reflex (attachWith, current, ffor, fforMaybeCheap, fmapMaybeCheap, getPostBuild, holdDyn, zipDynWith)
import Reflex.Native
  ( MonadNative, ViewBuilder(recognizeGesture, wrapRawView), ViewBuilderSpace, ViewSpace(type RawView, containerViewAsView)
  , container_, containerWith, text_, accessibilityLabel, backgroundColor
  , GestureSpec(..), GestureState(..), PanGesture(..), _gestureState_data
  , Point(..), Rect(..), Size(..), lightGray, darkGray
  , RawViewConfig(..)
  , RootLayout
  , ViewConstraint(..), (==@), centerOf, dynT, horizontalConstraint, lengthOf, verticalConstraint
  , ViewStyle(..)
  )

main :: forall t m. MonadNative t RootLayout m => RawView t (ViewBuilderSpace m) RootLayout -> m ()
main rootRawView = do
  pb <- getPostBuild
  let rootViewConfig = def
        { _rawViewConfig_modifyStyle = Just $ (def @(ViewStyle Identity))
            { _viewStyle_backgroundColor = pb $> lightGray }
        }
  _rootView <- wrapRawView rootRawView rootViewConfig

  container_ $ do
    rec
      let pos0 = Point 10 10
      dragBlock <- fmap (containerViewAsView . snd) . containerWith (backgroundColor darkGray <> accessibilityLabel "test view") $ text_ "drag me!"
      horizontalConstraint $ lengthOf dragBlock ==@ 100
      verticalConstraint $ lengthOf dragBlock ==@ 100
      horizontalConstraint $ centerOf dragBlock ==@ dynT (_point_x <$> pos)
      verticalConstraint $ centerOf dragBlock ==@ dynT (_point_y <$> pos)

      panState <- recognizeGesture dragBlock GestureSpec_Pan
      lastStartPos <- holdDyn pos0 $ attachWith const (current pos) $ fforMaybeCheap panState $ \ case
        GestureState_Began _ -> Just ()
        _                    -> Nothing
      lastDragTranslation <- holdDyn zeroV $ fmapMaybeCheap (fmap _panGesture_translation . _gestureState_data) panState
      let pos = zipDynWith (^+^) lastStartPos lastDragTranslation

    pure ()


