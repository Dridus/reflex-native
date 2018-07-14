{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | DSL for representing 'Constraint's on 'Expression's composed of 'Term's and variables.
--
-- For example (from the Cassowary technical report):
--
-- @
--   xl <- variable "xl"
--   xm <- editVariable strong "xm"
--   xr <- variable "xr"
--   constrain 'required' $ 'varT' xm '*.' 2 '==@' 'varT' xl '+:' 'varT' xr
--   constrain 'required' $ 'varT' xl '+:' 10 '<=@' 'varT' xr
--   constrain 'required' $ 'varT' xr '<=@' 100
--   constrain 'required' $ 0 '<=@' 'varT' xl
-- @
--
-- == Operator naming
--
-- Naming is always a challenge and operator naming doubly so, and in order to avoid type ambiguities the various operators are distinguished by what type they
-- produce:
--
--      * Operators that produce an 'Term' are denoted with a @.@, e.g. @*.@, @/.@.
--      * Operators that produce an 'Expression' are denoted with a @:@, e.g. @*:@, @/:@.
--      * Operators that produce a 'Constraint' are denoted with a @@@, e.g. @==@@.
--
-- Named versions of the operators are also provided and follow a similar convention: functions which produce an 'Term' end with @T@, e.g. 'mulT',
-- 'negateT'; functions which produce an 'Expression' end with @E@, e.g. 'mulE', 'addE'.
module Kiwi.Dsl
  (
  -- * Terms of an expression
  -- ** 'Term' and implicit promotion
    Term(..)
  -- ** Operators to construct terms
  , varT, (*.), mulT, (/.), divT, negateT
  -- * Expressions
  -- ** 'Expression' and implicit promotion
  , Expression(..), AsExpression(..)
  -- ** Operators to construct expressions
  , (*:), mulE, (/:), divE, (+:), addE, (-:), subE, negateE, constE
  -- * Constraints
  , Constraint(..), RelationalOperator(..), (==@), eqC, (<=@), leC, (>=@), geC
  -- * Constraint Strength
  -- ** 'Strength', deconstructor and constructor
  , Strength, rawStrength, unStrength, mkStrength, mkStrengthWeighted
  -- ** Constant values
  , required, strong, medium, weak
  ) where

import GHC.Generics (Generic)


-- |A single term in a linear expression 'Expression' of the form @v * f@ where @v@ is some variable type @v@ and @f@ is a Double coefficient. Constructed
-- implicitly by promoting a variable by way of 'AsTerm', or using '(*.)' ('mulT') or '(/.)' ('divT').
data Term v = Term
  { _exprTerm_variable :: v
  -- ^The variable in the term (@v@ in @v * f@)
  , _exprTerm_coefficient :: Double
  -- ^The coefficient in the term (@f@ in @v * f@)
  } deriving (Eq, Functor, Generic)

instance Show v => Show (Term v) where
  showsPrec d (Term v f) = showParen (d > 7) $ showsPrec 8 v . showString " *. " . showsPrec 8 f

infixl 7 *., /.

-- |Introduce a variable as a 'Term' with a coefficient of 1.
varT :: v -> Term v
varT v = Term v 1

-- |Multiply a term by a coefficient to make a 'Term'. Equivalent to '(*.)' or 'Term'.
mulT :: Term v -> Double -> Term v
mulT = (*.)

-- |Multiply a term by a coefficient to make a 'Term'. Equivalent to 'mulT' or 'Term'.
(*.) :: Term v -> Double -> Term v
Term v f *. g = Term v (f*g)

-- |Divide a term by a coefficient to make a 'Term'. Equivalent to '(/.)' or @'(*.)' . (1/)@.
divT :: Term v -> Double -> Term v
divT = (/.)

-- |Divide a term by a coefficient to make a 'Term'. Equivalent to 'divT' or @'(*.)' . (1/)@.
(/.) :: Term v -> Double -> Term v
t /. f = t *. (1/f)

-- |Negate a variable by multiplying it by @-1@.
negateT :: Term v -> Term v
negateT (Term v f) = Term v (negate f)

-- |A linear expression of the form @v1*f1 + v2*f2 + … + c@ where @vN@ are variables, @fN@ are coefficients, and @c@ is a constant. Constructed implicitly
-- by promoting a 'Double', @v@, or @'Term' v@ by way of @'AsExpression' v@, or using '(*:)' ('mulE'), '(/:)' ('divE'), '(+:)' ('addE'), or '(-:)' ('subE').
data Expression v = Expression
  { _expression_terms :: [Term v]
  -- ^The terms of the expression (@vN*fN@ in @v1*f1 + v2*f2 + … + c@)
  , _expression_constant :: Double
  -- ^The constant of the expression (@c@ in @v1*f1 + v2*f2 + … + c@)
  } deriving (Eq, Functor, Generic)

instance Show v => Show (Expression v) where
  showsPrec d (Expression ts c) =
    showParen (d > 6)
      $ foldr (\ t r -> showsPrec 7 t . showString " +: " . r) id ts
      . showString "constE "
      . showsPrec 7 c

-- |Class of things ('Expression's, 'Term's, and constants) which can be trivially converted to an 'Expression' for building with 'expression'.
class AsExpression v a | a -> v where
  asExpression :: a -> Expression v

-- |'Expression's are trivially 'Expression's
instance AsExpression v (Expression v) where
  asExpression = id

-- |'Term's can be 'Expression's by adding 0, i.e. @t + 0@
instance AsExpression v (Term v) where
  asExpression t = Expression [t] 0

infixl 7 *:, /:
infixl 6 +:, -:

-- |Multiply an 'Expression', 'Term', or constant 'Double' by a scalar multiple to make an 'Expression'. Equivalent to '(*:)'.
-- Promotes the left hand side to an 'Expression' using 'AsExpression' then multiplies each term's coefficient and the constant by the given scalar.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     mulE :: Expression v -> Double -> Expression v
--     mulE :: Term v       -> Double -> Expression v
-- @
mulE :: AsExpression v a => a -> Double -> Expression v
mulE = (*:)

-- |Multiply an 'Expression', 'Term', variable, or 'Double' by a scalar multiple to make an 'Expression'. Equivalent to 'mulE'.
-- Promotes the left hand side to an 'Expression' using 'AsExpression' then multiplies each term's coefficient and the constant by the given scalar.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (*:) :: Expression v -> Double -> Expression v
--     (*:) :: Term v       -> Double -> Expression v
-- @
(*:) :: AsExpression v a => a -> Double -> Expression v
a *: s =
  let Expression ts c = asExpression a
  in Expression (map (*. s) ts) (c * s)

-- |Divide an 'Expression', 'Term', variable, or 'Double' by a scalar divisor to make an 'Expression'. Equivalent to '(/:)'.
-- Promotes the left hand side to an 'Expression' using 'AsExpression' then multiplies each term's coefficient and the constant by the reciprocal.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     divE :: Expression v -> Double -> Expression v
--     divE :: Term v       -> Double -> Expression v
-- @
divE :: AsExpression v a => a -> Double -> Expression v
divE = (/:)

-- |Divide an 'Expression', 'Term', variable, or 'Double' by a scalar divisor to make an 'Expression'. Equivalent to 'divE'.
-- Promotes the left hand side to an 'Expression' using 'AsExpression' then multiplies each term's coefficient and the constant by the reciprocal.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (/:) :: Expression v -> Double -> Expression v
--     (/:) :: Term v       -> Double -> Expression v
-- @
(/:) :: AsExpression v a => a -> Double -> Expression v
a /: s =
  let Expression ts c = asExpression a
  in Expression (map (/. s) ts) (c / s)


-- |Add an 'Expression', 'Term', variable, or 'Double' to another 'Expression', 'Term', variable, or 'Double' to make an 'Expression'. Equivalent to '(+:)'.
-- Promotes both sides to 'Expression's using 'AsExpression' then concatenates the terms and adds the constants of each to form the result.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     addE :: Expression v -> Expression v -> Expression v
--     addE :: Term v       -> Expression v -> Expression v
--     addE :: Expression v -> Term v       -> Expression v
--     addE :: Term v       -> Term v       -> Expression v
--     addE :: Expression v -> Double       -> Expression v
--     addE :: Term v       -> Double       -> Expression v
-- @
addE :: (AsExpression v a, AsExpression v b) => a -> b -> Expression v
addE = (+:)

-- |Add an 'Expression', 'Term', variable, or 'Double' to another 'Expression', 'Term', variable, or 'Double' to make an 'Expression'. Equivalent to 'addE'.
-- Promotes both sides to 'Expression's using 'AsExpression' then concatenates the terms and adds the constants of each to form the result.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (+:) :: Expression v -> Expression v -> Expression v
--     (+:) :: Term v       -> Expression v -> Expression v
--     (+:) :: Expression v -> Term v       -> Expression v
--     (+:) :: Term v       -> Term v       -> Expression v
--     (+:) :: Expression v -> Double       -> Expression v
--     (+:) :: Term v       -> Double       -> Expression v
-- @
(+:) :: (AsExpression v a, AsExpression v b) => a -> b -> Expression v
a +: b =
  let Expression tsa ca = asExpression a
      Expression tsb cb = asExpression b
  in Expression (tsa ++ tsb) (ca + cb)

-- |Subtract an 'Expression', 'Term', variable, or 'Double' from another 'Expression', 'Term', variable, or 'Double' to make an 'Expression'. Equivalent to '(-:)'.
-- Promotes both sides to 'Expression's using 'AsExpression' then adds ('(+:)' / 'addE') the left to the negated ('negateE') right.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     subE :: Expression v -> Expression v -> Expression v
--     subE :: Term v       -> Expression v -> Expression v
--     subE :: Expression v -> Term v       -> Expression v
--     subE :: Term v       -> Term v       -> Expression v
--     subE :: Expression v -> Double       -> Expression v
--     subE :: Term v       -> Double       -> Expression v
-- @
subE :: (AsExpression v a, AsExpression v b) => a -> b -> Expression v
subE = (-:)

-- |Subtract an 'Expression', 'Term', variable, or 'Double' from another 'Expression', 'Term', variable, or 'Double' to make an 'Expression'. Equivalent to 'subE'.
-- Promotes both sides to 'Expression's using 'AsExpression' then adds ('(+:)' / 'addE') the left to the negated ('negateE') right.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (-:) :: Expression v -> Expression v -> Expression v
--     (-:) :: Term v       -> Expression v -> Expression v
--     (-:) :: Expression v -> Term v       -> Expression v
--     (-:) :: Term v       -> Term v       -> Expression v
--     (-:) :: Expression v -> Double       -> Expression v
--     (-:) :: Term v       -> Double       -> Expression v
-- @
(-:) :: (AsExpression v a, AsExpression v b) => a -> b -> Expression v
a -: b = a +: negateE b

-- |Negate an 'Expression', 'Term', variable, or 'Double' to produce an 'Expression'. Promotes the value to 'Expression' by way of 'AsExpression' and then multiplies with @-1@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     negateE :: Expression v -> Expression v
--     negateE :: Term v       -> Expression v
-- @
negateE :: AsExpression v a => a -> Expression v
negateE = (*: (-1)) . asExpression

-- |Convert a literal 'Double' into an 'Expression'. Helper to fix types where they would be ambiguous, usually when adding a constant to an 'Expression'.
constE :: Double -> Expression v
constE = Expression []

-- |The constraint relation to establish between an expression and 0, i.e. the @=@, @≤@ or @≥@ in @e = 0@, @e ≤ 0@, @e ≥ 0@.
data RelationalOperator
  -- WARNING: do not reorder as the Enum values matter!
  = RelationalOperator_Le -- ^The expression should be ≤ 0 or as close as possible to 0 if the constraint is not of required strength.
  | RelationalOperator_Ge -- ^The expression should be ≥ 0 or as close as possible to 0 if the constraint is not of required strength.
  | RelationalOperator_Eq -- ^The expression should be = 0 or as close as possible if the constraint is not of required strength.
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

-- |A constraint that can be added to the constraint system and optimized, consisting of some 'Expression' and its intended relation to 0. The strength of
-- the constraint is established when it's converted into a 'Raw.Constraint'.
data Constraint v = Constraint
  { _constraint_expression :: Expression v
  -- ^The expression of the constraint, describing the intended relationship to satisfy.
  , _constraint_operator :: RelationalOperator
  -- ^The relation of the expression to 0.
  } deriving (Eq, Functor, Generic)

instance Show v => Show (Constraint v) where
  showsPrec d (Constraint e o) =
    showParen (d > 4)
      $ showsPrec 5 e
      . showString (case o of { RelationalOperator_Eq -> " ==@ 0"; RelationalOperator_Le -> " <=@ 0"; RelationalOperator_Ge -> " >=@ 0" })

infixl 4 ==@, <=@, >=@

-- |Create a constraint of equality between two 'Expression's, 'Term's, variables, or 'Double's. Equivalent to '(==@)'.
-- Promotes both sides via 'AsExpression' then creates a constraint that @left - right == 0@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     eqC :: Expression v -> Expression v -> Constraint v
--     eqC :: Term v       -> Expression v -> Constraint v
--     eqC :: Double       -> Expression v -> Constraint v
--     eqC :: Expression v -> Term v       -> Constraint v
--     eqC :: Term v       -> Term v       -> Constraint v
--     eqC :: Double       -> Term v       -> Constraint v
--     eqC :: Expression v -> Double       -> Constraint v
--     eqC :: Term v       -> Double       -> Constraint v
--     eqC :: Double       -> Double       -> Constraint v
-- @
eqC :: (AsExpression v a, AsExpression v b) => a -> b -> Constraint v
eqC = (==@)

-- |Create a constraint of equality between two 'Expression's, 'Term's, variables, or 'Double's. Equivalent to 'eqC'.
-- Promotes both sides via 'AsExpression' then creates a constraint that @left - right == 0@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (==@) :: Expression v -> Expression v -> Constraint v
--     (==@) :: Term v       -> Expression v -> Constraint v
--     (==@) :: Double       -> Expression v -> Constraint v
--     (==@) :: Expression v -> Term v       -> Constraint v
--     (==@) :: Term v       -> Term v       -> Constraint v
--     (==@) :: Double       -> Term v       -> Constraint v
--     (==@) :: Expression v -> Double       -> Constraint v
--     (==@) :: Term v       -> Double       -> Constraint v
--     (==@) :: Double       -> Double       -> Constraint v
-- @
(==@) :: (AsExpression v a, AsExpression v b) => a -> b -> Constraint v
a ==@ b = Constraint (a -: b) RelationalOperator_Eq

-- |Create a constraint of equality between two 'Expression's, 'Term's, variables, or 'Double's. Equivalent to '(<=@)'.
-- Promotes both sides via 'AsExpression' then creates a constraint that @left - right <= 0@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     leC :: Expression v -> Expression v -> Constraint v
--     leC :: Term v       -> Expression v -> Constraint v
--     leC :: Double       -> Expression v -> Constraint v
--     leC :: Expression v -> Term v       -> Constraint v
--     leC :: Term v       -> Term v       -> Constraint v
--     leC :: Double       -> Term v       -> Constraint v
--     leC :: Expression v -> Double       -> Constraint v
--     leC :: Term v       -> Double       -> Constraint v
--     leC :: Double       -> Double       -> Constraint v
-- @
leC :: (AsExpression v a, AsExpression v b) => a -> b -> Constraint v
leC = (<=@)

-- |Create a constraint of equality between two 'Expression's, 'Term's, variables, or 'Double's. Equivalent to 'leC'.
-- Promotes both sides via 'AsExpression' then creates a constraint that @left - right <= 0@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (<=@) :: Expression v -> Expression v -> Constraint v
--     (<=@) :: Term v       -> Expression v -> Constraint v
--     (<=@) :: Double       -> Expression v -> Constraint v
--     (<=@) :: Expression v -> Term v       -> Constraint v
--     (<=@) :: Term v       -> Term v       -> Constraint v
--     (<=@) :: Double       -> Term v       -> Constraint v
--     (<=@) :: Expression v -> Double       -> Constraint v
--     (<=@) :: Term v       -> Double       -> Constraint v
--     (<=@) :: Double       -> Double       -> Constraint v
-- @
(<=@) :: (AsExpression v a, AsExpression v b) => a -> b -> Constraint v
a <=@ b = Constraint (a -: b) RelationalOperator_Le

-- |Create a constraint of equality between two 'Expression's, 'Term's, variables, or 'Double's. Equivalent to '(>=@)'.
-- Promotes both sides via 'AsExpression' then creates a constraint that @left - right >= 0@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     geC :: Expression v -> Expression v -> Constraint v
--     geC :: Term v       -> Expression v -> Constraint v
--     geC :: Double       -> Expression v -> Constraint v
--     geC :: Expression v -> Term v       -> Constraint v
--     geC :: Term v       -> Term v       -> Constraint v
--     geC :: Double       -> Term v       -> Constraint v
--     geC :: Expression v -> Double       -> Constraint v
--     geC :: Term v       -> Double       -> Constraint v
--     geC :: Double       -> Double       -> Constraint v
-- @
geC :: (AsExpression v a, AsExpression v b) => a -> b -> Constraint v
geC = (>=@)

-- |Create a constraint of equality between two 'Expression's, 'Term's, variables, or 'Double's. Equivalent to 'geC'.
-- Promotes both sides via 'AsExpression' then creates a constraint that @left - right >= 0@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (>=@) :: Expression v -> Expression v -> Constraint v
--     (>=@) :: Term v       -> Expression v -> Constraint v
--     (>=@) :: Double       -> Expression v -> Constraint v
--     (>=@) :: Expression v -> Term v       -> Constraint v
--     (>=@) :: Term v       -> Term v       -> Constraint v
--     (>=@) :: Double       -> Term v       -> Constraint v
--     (>=@) :: Expression v -> Double       -> Constraint v
--     (>=@) :: Term v       -> Double       -> Constraint v
--     (>=@) :: Double       -> Double       -> Constraint v
-- @
(>=@) :: (AsExpression v a, AsExpression v b) => a -> b -> Constraint v
a >=@ b = Constraint (a -: b) RelationalOperator_Ge

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

