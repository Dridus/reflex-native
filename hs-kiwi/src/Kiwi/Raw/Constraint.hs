{-# LANGUAGE DeriveGeneric #-}
-- |Methods of Kiwi's @Constraint@ class, which represents a constraint the solver either must or should try to adhere to and optimize in the form @e = 0@,
-- @e ≤ 0@, or @e ≥ 0@, where @e@ is an 'Expression' composed of 'Kiwi.Raw.Types.Term's along with a strength to prioritize the constraint relative to others.
module Kiwi.Raw.Constraint
  (
  -- * Relational operator type
    RelationalOperator(..)

  -- * Creating 'Constraint's
  , new

  -- * Accessor functions
  , getExpression, getOperator, getStrength

  -- * Raw FFI bindings
  , kiwiConstraint_new, kiwiConstraint_free, kiwiConstraint_getExpression, kiwiConstraint_getOperator, kiwiConstraint_getStrength
  ) where

import Control.Monad ((=<<))
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import GHC.Generics (Generic)
import Kiwi.Raw.Expression (kiwiExpression_free)
import Kiwi.Raw.Strength (Strength, rawStrength, unStrength)
import Kiwi.Raw.Types (Constraint, ConstraintType, Expression, ExpressionType)


-- |The constraint relation to establish between an expression and 0, i.e. the @=@, @≤@ or @≥@ in @e = 0@, @e ≤ 0@, @e ≥ 0@.
data RelationalOperator
  -- WARNING: make sure to keep in sync with the C++ header so the Enum values match!
  = RelationalOperator_Le -- ^The expression should be ≤ 0 or as close as possible to 0 if the constraint is not of required strength.
  | RelationalOperator_Ge -- ^The expression should be ≥ 0 or as close as possible to 0 if the constraint is not of required strength.
  | RelationalOperator_Eq -- ^The expression should be = 0 or as close as possible if the constraint is not of required strength.
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

-- |Create a new 'Constraint' by giving the 'Expression', its relation to 0, and the strength of the constraint.
new :: Expression -> RelationalOperator -> Strength -> IO Constraint
new exprFp op strength =
  withForeignPtr exprFp $ \ expr -> do
    newForeignPtr kiwiConstraint_free =<< kiwiConstraint_new expr (fromIntegral $ fromEnum op) (unStrength strength)

-- |Get the 'Expression' part of a 'Constraint'.
getExpression :: Constraint -> IO Expression
getExpression constraintFp =
  withForeignPtr constraintFp $ \ constraintP ->
    newForeignPtr kiwiExpression_free =<< kiwiConstraint_getExpression constraintP

-- |Get the operator relating the 'Constraint's expression to 0.
getOperator :: Constraint -> IO RelationalOperator
getOperator constraintFp =
  toEnum . fromIntegral <$> withForeignPtr constraintFp kiwiConstraint_getOperator

-- |Get the strength of the 'Constraint' in the constraint system.
getStrength :: Constraint -> IO Strength
getStrength constraintFp =
  rawStrength <$> withForeignPtr constraintFp kiwiConstraint_getStrength

-- |Raw FFI binding to @new Constraint(Expression&, RelationalOperator, double)@
foreign import ccall unsafe kiwiConstraint_new :: Ptr ExpressionType -> CInt -> Double -> IO (Ptr ConstraintType)
-- |Raw FFI binding to @delete@ for @Constraint*@
foreign import ccall unsafe "&kiwiConstraint_free" kiwiConstraint_free :: FunPtr (Ptr ConstraintType -> IO ())
-- |Raw FFI binding to @Expression& Constraint::expression() const@
foreign import ccall unsafe kiwiConstraint_getExpression :: Ptr ConstraintType -> IO (Ptr ExpressionType)
-- |Raw FFI binding to @RelationalOperator Constraint::op() const@
foreign import ccall unsafe kiwiConstraint_getOperator :: Ptr ConstraintType -> IO CInt
-- |Raw FFI binding to @double Constraint::strength() const@
foreign import ccall unsafe kiwiConstraint_getStrength :: Ptr ConstraintType -> IO Double

