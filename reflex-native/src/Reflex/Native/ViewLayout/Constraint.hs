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

data ViewVariable axis space t where
  ViewVariable_ViewProperty :: forall axis space t. View space ConstraintLayout t -> ViewProperty axis -> ViewVariable axis space t
  ViewVariable_Dynamic :: forall axis space t. Dynamic t Double -> ViewVariable axis space t

instance Show (View space ConstraintLayout t) => Show (ViewVariable axis space t) where
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

data ViewConstraint space t
  = ViewConstraint_Horizontal (Constraint (ViewVariable 'Horizontal space t))
  | ViewConstraint_Vertical (Constraint (ViewVariable 'Vertical space t))

instance Show (View space ConstraintLayout t) => Show (ViewConstraint space t) where
  showsPrec _ = \ case
    ViewConstraint_Horizontal c -> showString "H: " . shows c
    ViewConstraint_Vertical c -> showString "V: " . shows c

dynT :: Dynamic t Double -> ViewVariable axis space t
dynT = ViewVariable_Dynamic

leftOf :: View space ConstraintLayout t -> ViewVariable 'Horizontal space t
leftOf = flip ViewVariable_ViewProperty ViewProperty_Left

leadingOf :: View space ConstraintLayout t -> ViewVariable 'Horizontal space t
leadingOf = flip ViewVariable_ViewProperty ViewProperty_Leading

rightOf :: View space ConstraintLayout t -> ViewVariable 'Horizontal space t
rightOf = flip ViewVariable_ViewProperty ViewProperty_Right

trailingOf :: View space ConstraintLayout t -> ViewVariable 'Horizontal space t
trailingOf = flip ViewVariable_ViewProperty ViewProperty_Trailing

topOf :: View space ConstraintLayout t -> ViewVariable 'Vertical space t
topOf = flip ViewVariable_ViewProperty ViewProperty_Top

bottomOf :: View space ConstraintLayout t -> ViewVariable 'Vertical space t
bottomOf = flip ViewVariable_ViewProperty ViewProperty_Bottom

centerOf :: View space ConstraintLayout t -> ViewVariable axis space t
centerOf = flip ViewVariable_ViewProperty ViewProperty_Center

lengthOf :: View space ConstraintLayout t -> ViewVariable axis space t
lengthOf = flip ViewVariable_ViewProperty ViewProperty_Length

data ConstraintLayout

instance ViewLayout ConstraintLayout t where
  data ContainerLayout ConstraintLayout t = ContainerLayout_Constraint
  data ContentLayout ConstraintLayout t = ContentLayout_Constraint

instance Default (ContainerLayout ConstraintLayout t) where
  def = ContainerLayout_Constraint
instance Default (ContentLayout ConstraintLayout t) where
  def = ContentLayout_Constraint

class (Eq (ViewConstraintId m), Ord (ViewConstraintId m), ViewSpace space) => ConstraintLayoutBuilder space t m | m -> space, m -> t where
  data ViewConstraintId m :: *

  addConstraint :: ViewConstraint space t -> m (ViewConstraintId m)
  incrementalConstraints :: Ord k => Incremental t (PatchMap k (ViewConstraint space t)) -> m ()
  removeConstraint :: ViewConstraintId m -> m ()

  horizontalConstraint :: Constraint (ViewVariable 'Horizontal space t) -> m (ViewConstraintId m)
  horizontalConstraint = addConstraint . ViewConstraint_Horizontal

  verticalConstraint :: Constraint (ViewVariable 'Vertical space t) -> m (ViewConstraintId m)
  verticalConstraint = addConstraint . ViewConstraint_Vertical
