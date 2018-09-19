{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Native.ViewLayout.Constraint
  ( ViewProperty(..)
  , ViewVariable(..), dynT, leftOf, leadingOf, rightOf, trailingOf, topOf, bottomOf, centerOf, lengthOf
  , ViewConstraint(..)
  , ConstraintLayout, ContainerLayout(ContainerLayout_Constraint), ContentLayout(ContentLayout_Constraint)
  , ConstraintLayoutBuilder(..)
  ) where

import Data.Default (Default(def))
import Kiwi.Dsl (Constraint)
import Reflex.Class (Dynamic, Incremental)
import Reflex.Native.Geometry (Axis(Horizontal, Vertical))
import Reflex.Native.ViewBuilder.Class (View, ViewSpace)
import Reflex.Native.ViewLayout.Class (ViewLayout(type ContainerLayout, type ContentLayout))
import Reflex.Patch.Map (PatchMap)

data ViewProperty (axis :: Axis) where
  ViewProperty_Left     :: ViewProperty 'Horizontal
  ViewProperty_Leading  :: ViewProperty 'Horizontal
  ViewProperty_Right    :: ViewProperty 'Horizontal
  ViewProperty_Trailing :: ViewProperty 'Horizontal

  ViewProperty_Top    :: ViewProperty 'Vertical
  ViewProperty_Bottom :: ViewProperty 'Vertical

  ViewProperty_Center :: ViewProperty axis
  ViewProperty_Length :: ViewProperty axis

data ViewVariable t space axis where
  ViewVariable_ViewProperty :: forall t space axis. View t space ConstraintLayout -> ViewProperty axis -> ViewVariable t space axis
  ViewVariable_Dynamic :: forall t space axis. Dynamic t Double -> ViewVariable t space axis

instance Show (View t space ConstraintLayout) => Show (ViewVariable t space axis) where
  showsPrec _ = \ case
    ViewVariable_ViewProperty v p ->
      let propStr = case p of
            ViewProperty_Left     -> "left of "
            ViewProperty_Leading  -> "leading edge of "
            ViewProperty_Right    -> "right of "
            ViewProperty_Trailing -> "trailing edge of "
            ViewProperty_Top      -> "top of "
            ViewProperty_Bottom   -> "bottom of "
            ViewProperty_Center   -> "center of "
            ViewProperty_Length   -> "length of "
      in showString propStr . shows v
    ViewVariable_Dynamic _ -> showString "<dynamic>"

data ViewConstraint t space
  = ViewConstraint_Horizontal (Constraint (ViewVariable t space 'Horizontal))
  | ViewConstraint_Vertical (Constraint (ViewVariable t space 'Vertical))

instance Show (View t space ConstraintLayout) => Show (ViewConstraint t space) where
  showsPrec _ = \ case
    ViewConstraint_Horizontal c -> showString "H: " . shows c
    ViewConstraint_Vertical c -> showString "V: " . shows c

dynT :: Dynamic t Double -> ViewVariable t space axis
dynT = ViewVariable_Dynamic

leftOf :: View t space ConstraintLayout -> ViewVariable t space 'Horizontal
leftOf = flip ViewVariable_ViewProperty ViewProperty_Left

leadingOf :: View t space ConstraintLayout -> ViewVariable t space 'Horizontal
leadingOf = flip ViewVariable_ViewProperty ViewProperty_Leading

rightOf :: View t space ConstraintLayout -> ViewVariable t space 'Horizontal
rightOf = flip ViewVariable_ViewProperty ViewProperty_Right

trailingOf :: View t space ConstraintLayout -> ViewVariable t space 'Horizontal
trailingOf = flip ViewVariable_ViewProperty ViewProperty_Trailing

topOf :: View t space ConstraintLayout -> ViewVariable t space 'Vertical
topOf = flip ViewVariable_ViewProperty ViewProperty_Top

bottomOf :: View t space ConstraintLayout -> ViewVariable t space 'Vertical
bottomOf = flip ViewVariable_ViewProperty ViewProperty_Bottom

centerOf :: View t space ConstraintLayout -> ViewVariable t space axis
centerOf = flip ViewVariable_ViewProperty ViewProperty_Center

lengthOf :: View t space ConstraintLayout -> ViewVariable t space axis
lengthOf = flip ViewVariable_ViewProperty ViewProperty_Length

data ConstraintLayout

instance ViewLayout t ConstraintLayout where
  data ContainerLayout t ConstraintLayout = ContainerLayout_Constraint
  data ContentLayout t ConstraintLayout = ContentLayout_Constraint

instance Default (ContainerLayout t ConstraintLayout) where
  def = ContainerLayout_Constraint
instance Default (ContentLayout t ConstraintLayout) where
  def = ContentLayout_Constraint

class (Eq (ViewConstraintId m), Ord (ViewConstraintId m), ViewSpace space) => ConstraintLayoutBuilder t space m | m -> space, m -> t where
  data ViewConstraintId m :: *

  addConstraint :: ViewConstraint t space -> m (ViewConstraintId m)
  incrementalConstraints :: Ord k => Incremental t (PatchMap k (ViewConstraint t space)) -> m ()
  removeConstraint :: ViewConstraintId m -> m ()

  horizontalConstraint :: Constraint (ViewVariable t space 'Horizontal) -> m (ViewConstraintId m)
  horizontalConstraint = addConstraint . ViewConstraint_Horizontal

  verticalConstraint :: Constraint (ViewVariable t space 'Vertical) -> m (ViewConstraintId m)
  verticalConstraint = addConstraint . ViewConstraint_Vertical
