{-# LANGUAGE OverloadedStrings #-}
-- |Pointer phantom types and pointer type aliases representing each of the types in Kiwi.
module Kiwi.Raw.Types
  (
  -- * Pointer types
    Constraint, Expression, Solver, Term
  -- * Phantom types
  , ConstraintType, ExpressionType, SolverType, TermType, VariableType
  -- * Variable
  , Variable(..)
  ) where

import Data.Text (Text, unpack)
import Data.Void (Void)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)


-- |Pointers to @kiwi::Constraint@.
type Constraint = ForeignPtr ConstraintType
-- |Pointers to @kiwi::Expression@.
type Expression = ForeignPtr ExpressionType
-- |Pointers to @kiwi::Solver@.
type Solver = ForeignPtr SolverType
-- |Pointers to @kiwi::Term@.
type Term = ForeignPtr TermType

-- |Phantom type for pointers to @kiwi::Constraint@
data ConstraintType
-- |Phantom type for pointers to @kiwi::Expression@
data ExpressionType
-- |Phantom type for pointers to @kiwi::Solver@
data SolverType
-- |Phantom type for pointers to @kiwi::Term@
data TermType
-- |Phantom type for pointers to @kiwi::Variable@
data VariableType

-- |A 'Variable' in the solver constraint system. This one is special because we really want symbolic equality using the unique identity of the variable instead
-- of which handle we have to it, and we want to be able to get the name without IO. So, we treat all variables as having a name set only at initialization time
-- and grab both the name and identity and stash them instead of deferring everything to FFI calls.
data Variable = Variable
  { _variable_ptr :: ForeignPtr VariableType
  -- ^The 'ForeignPtr' to the Kiwi @Variable@ instance.
  , _variable_identity :: Ptr Void
  -- ^External pointer to the Kiwi @VariableData@ that establishes the unique identity of this variable, no matter how many copies of the @Variable@ handle
  -- exist.
  , _variable_name :: Text
  -- ^The name of the variable. May be empty.
  }
instance Eq Variable where
  Variable _ a _ == Variable _ b _ = a == b
instance Show Variable where
  show (Variable _ i "") = "variable " ++ show i
  show (Variable _ _ n ) = unpack n

