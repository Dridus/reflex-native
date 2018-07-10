-- |Type representing the strength of a constraint in the constraint system and standard strength constants, ranging from required on down.
module Kiwi.Raw.Strength
  (
  -- * 'Strength', deconstructor and constructor
    Strength, rawStrength, unStrength, mkStrength, mkStrengthWeighted
  -- * Constant values
  , required, strong, medium, weak
  ) where


newtype Strength = Strength { unStrength :: Double }
  deriving (Eq, Ord, Show)

instance Bounded Strength where
  minBound = Strength 0
  maxBound = required

-- |Construct a 'Strength' value directly from a 'Double' without bounds checking.
{-# INLINE rawStrength #-}
rawStrength :: Double -> Strength
rawStrength = Strength

-- |Construct a 'Strength' value by combining a strong, medium, and weak values. Equivalent to 'mkStrengthWeighted' with a weight of @1.0@.
{-# INLINE mkStrength #-}
mkStrength
  :: Double -- ^The strong value, ranging [0.0 .. 1000.0]
  -> Double -- ^The medium value, ranging [0.0 .. 1000.0]
  -> Double -- ^The weak value, ranging [0.0 .. 1000.0]
  -> Strength
mkStrength a b c = mkStrengthWeighted a b c 1.0

-- |Construct a 'Strength' value by combining a strong, medium, and weak values plus a weight (usually @1.0@).
{-# INLINE mkStrengthWeighted #-}
mkStrengthWeighted
  :: Double -- ^The strong value, ranging [0.0 .. 1000.0]. Out of bounds values after the weight is applied will be clamped.
  -> Double -- ^The medium value, ranging [0.0 .. 1000.0]. Out of bounds values after the weight is applied will be clamped.
  -> Double -- ^The weak value, ranging [0.0 .. 1000.0]. Out of bounds values after the weight is applied will be clamped.
  -> Double -- ^The weight factor which applies to each of the values, usually @1.0@ (i.e. no alteration of weight)
  -> Strength
mkStrengthWeighted a b c w = Strength $ clamp (a*w) * 1000000 + clamp (b*w) * 1000 + clamp (c*w)
  where clamp = max 0 . min 1000

-- |The required strength, indicating a constraint must be met or the constraint system is unsolvable.
--
-- Equivalent to @'mkStrength' 1000 1000 1000@.
required :: Strength
required = mkStrength 1000 1000 1000

-- |The strong strength.
--
-- Equivalent to @'mkStrength' 1 0 0@.
strong :: Strength
strong = mkStrength 1000 0 0

-- |The medium strength.
--
-- Equivalent to @'mkStrength' 0 1 0@.
medium :: Strength
medium = mkStrength 0 1000 0

-- |The weak strength.
--
-- Equivalent to @'mkStrength' 0 0 1@.
weak :: Strength
weak = mkStrength 0 0 1000

