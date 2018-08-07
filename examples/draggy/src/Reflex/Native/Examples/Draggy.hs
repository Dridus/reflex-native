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
import Data.Functor (($>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Reflex (attachWith, current, ffor, fforMaybeCheap, fmapMaybeCheap, getPostBuild, holdDyn, zipDynWith)
import Reflex.Native
  ( MonadNative, ViewBuilder(type ViewBuilderSpace, recognizeGesture, wrapRawView), ViewSpace(type RawView)
  , containerWith, textWith_, accessibilityLabel, backgroundColor, incrementalLayout
  , GestureSpec(..), GestureState(..), PanGesture(..), _gestureState_data
  , Point(..), Rect(..), Size(..), lightGray, darkGray
  , RawViewConfig(..), defaultRawViewConfig
  , ViewConstraint(..), (==@), centerOf, dynT, lengthOf
  , ViewStyle(..), defaultModifyViewStyle
  )

main :: forall t m. MonadNative t m => RawView (ViewBuilderSpace m) -> m ()
main rootRawView = do
  pb <- getPostBuild
  let rootViewConfig = defaultRawViewConfig
        { _rawViewConfig_modifyStyle = Just $ (defaultModifyViewStyle @t)
            { _viewStyle_backgroundColor = pb $> lightGray }
        }
  _rootView <- wrapRawView rootRawView rootViewConfig

  rec
    let pos0 = Point 10 10
        layout = Map.fromList
          ]
    container_ $
        (_, dragBlock) <- containerWith (backgroundColor darkGray <> accessibilityLabel "test view") $ text_ "drag me!"
        horizontalConstraint $ lengthOf dragBlock ==@ 100
        verticalConstraint $ lengthOf dragBlock ==@ 100
        horizontalConstraint $ centerOf dragBlock ==@ dynT (_point_x <$> pos)
        verticalConstraint $ centerOf dragBlock ==@ dynT (_point_y <$> pos)
    panState <- recognizeGesture vn GestureSpec_Pan
    lastStartPos <- holdDyn pos0 $ attachWith const (current pos) $ fforMaybeCheap panState $ \ case
      GestureState_Began _ -> Just ()
      _                    -> Nothing
    lastDragTranslation <- holdDyn zeroV $ fmapMaybeCheap (fmap _panGesture_translation . _gestureState_data) panState
    let pos = zipDynWith (^+^) lastStartPos lastDragTranslation

  pure ()



