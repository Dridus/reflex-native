-- |Methods of the Kiwi @Term@ class, which are linear terms of the form @v*f@ for some 'Kiwi.Cpp.Raw.Variable' @v@ and coefficient @f@.
-- Used in an 'Kiwi.Cpp.Raw.Expression' used to express constraints to solve.
module Kiwi.Cpp.Raw.Term
  (
  -- * Creating new 'Term's
    new

  -- * Accessor functions
  , getVariable, getCoefficient, getValue

  -- * Raw FFI bindings
  , kiwiTerm_new, kiwiTerm_free, kiwiTerm_getVariable, kiwiTerm_getCoefficient, kiwiTerm_getValue
  ) where

import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import Kiwi.Cpp.Raw.Types (Term, TermType, Variable(..), VariableType)
import qualified Kiwi.Cpp.Raw.Variable as Variable


-- |Create a new 'Term' by giving the 'Variable' and coefficient.
new :: Variable -> Double -> IO Term
new variable coef =
  withForeignPtr (_variable_ptr variable) $ \ varP ->
    newForeignPtr kiwiTerm_free =<< kiwiTerm_new varP coef

-- |Get the 'Variable' part of a 'Term'.
getVariable :: Term -> IO Variable
getVariable termFp =
  withForeignPtr termFp $ \ termP ->
    Variable.adopt =<< kiwiTerm_getVariable termP

-- |Get the coefficient of the 'Term'.
getCoefficient :: Term -> IO Double
getCoefficient termFp =
  withForeignPtr termFp $ \ termP ->
    kiwiTerm_getCoefficient termP

-- |Get the most recently updated solution for the 'Term'.
getValue :: Term -> IO Double
getValue termFp =
  withForeignPtr termFp $ \ termP ->
    kiwiTerm_getValue termP

-- |Raw binding to @Term::Term(const Variable&, double)@
foreign import ccall unsafe kiwiTerm_new :: Ptr VariableType -> Double -> IO (Ptr TermType)
-- |Raw binding to @delete@ for @Term*@
foreign import ccall unsafe "&kiwiTerm_free" kiwiTerm_free :: FunPtr (Ptr TermType -> IO ())
-- |Raw binding to @Variable& Term::variable() const@
foreign import ccall unsafe kiwiTerm_getVariable :: Ptr TermType -> IO (Ptr VariableType)
-- |Raw binding to @double Term::coefficient() const@
foreign import ccall unsafe kiwiTerm_getCoefficient :: Ptr TermType -> IO Double
-- |Raw binding to @double Term::value() const@
foreign import ccall unsafe kiwiTerm_getValue :: Ptr TermType -> IO Double


