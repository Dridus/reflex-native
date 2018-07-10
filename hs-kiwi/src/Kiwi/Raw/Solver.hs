-- |Methods of Kiwi's @Solver@ class, which represents the stateful incremental constraint solver to which 'Constraint's and edit 'Variable's can be added and
-- removed dynamically while computing the optimal solution of the constraint system.
module Kiwi.Raw.Solver
  (
  -- * Creating new 'Solver's
    new
  -- *
  , addConstraint, removeConstraint, hasConstraint
  , addEditVariable, removeEditVariable, hasEditVariable
  , suggestValue, updateVariables, reset, dump
  -- * Raw FFI bindings
  , kiwiSolver_new, kiwiSolver_free, kiwiSolver_addConstraint, kiwiSolver_removeConstraint, kiwiSolver_hasConstraint, kiwiSolver_addEditVariable
  , kiwiSolver_removeEditVariable, kiwiSolver_hasEditVariable, kiwiSolver_suggestValue, kiwiSolver_updateVariables, kiwiSolver_reset, kiwiSolver_dump
  ) where

import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import Kiwi.Raw.Errors (ErrorStructType, KiwiError, withErroringCall)
import Kiwi.Raw.Strength (Strength, unStrength)
import Kiwi.Raw.Types (Constraint, ConstraintType, Solver, SolverType, Variable(..), VariableType)


-- |Create a new 'Solver', empty of 'Constraint's and edit 'Variable's.
new :: IO Solver
new = newForeignPtr kiwiSolver_free =<< kiwiSolver_new

-- |Add a 'Constraint' to a 'Solver', updating the optimization solution.
--
-- May fail with:
--
--     * 'Kiwi.Raw.Errors.KiwiError_DuplicateConstraint' if the constraint is already in the system.
--     * 'Kiwi.Raw.Errors.KiwiError_UnsatisfiableConstraint' if the constraint would make the system unsatisfiable (i.e. conflicts with one or more existing
--     constraints).
--     * 'Kiwi.Raw.Errors.KiwiError_InternalSolverError' if something really sad happens internally.
addConstraint :: Solver -> Constraint -> IO (Either KiwiError ())
addConstraint solverFp constraintFp =
  withErroringCall $
    withForeignPtr solverFp $ \ solverP ->
    withForeignPtr constraintFp $ \ constraintP ->
      kiwiSolver_addConstraint solverP constraintP

-- |Remove a 'Constraint' from a 'Solver', updating the optimization solution.
--
-- May fail with:
--
--     * 'Kiwi.Raw.Errors.KiwiError_UnknownConstraint' if the constraint is not in the system.
--     * 'Kiwi.Raw.Errors.KiwiError_InternalSolverError' if something really sad happens internally.
removeConstraint :: Solver -> Constraint -> IO (Either KiwiError ())
removeConstraint solverFp constraintFp =
  withErroringCall $
    withForeignPtr solverFp $ \ solverP ->
    withForeignPtr constraintFp $ \ constraintP ->
      kiwiSolver_removeConstraint solverP constraintP

-- |Check if a given 'Constraint' is already in the constraint system of a 'Solver'.
hasConstraint :: Solver -> Constraint -> IO Bool
hasConstraint solverFp constraintFp =
  withForeignPtr solverFp $ \ solverP ->
  withForeignPtr constraintFp $ \ constraintP ->
    (/= 0) <$> kiwiSolver_hasConstraint solverP constraintP

-- |Add an edit 'Variable' to a 'Solver', updating the optimization solution.
--
-- May fail with:
--
--     * 'Kiwi.Raw.Errors.KiwiError_DuplicateEditVariable' if the constraint is already in the system.
--     * 'Kiwi.Raw.Errors.KiwiError_UnsatisfiableConstraint' if the constraint would make the system unsatisfiable (i.e. conflicts with one or more existing
--     constraints).
--     * 'Kiwi.Raw.Errors.KiwiError_BadRequiredStrength' if the strength given is required strength, which is not allowed for edit variables.
--     * 'Kiwi.Raw.Errors.KiwiError_InternalSolverError' if something really sad happens internally.
addEditVariable :: Solver -> Variable -> Strength -> IO (Either KiwiError ())
addEditVariable solverFp variable str =
  withErroringCall $
    withForeignPtr solverFp $ \ solverP ->
    withForeignPtr (_variable_ptr variable) $ \ variableP ->
      kiwiSolver_addEditVariable solverP variableP (unStrength str)

-- |Remove an edit 'Variable' from a 'Solver', updating the optimization solution.
--
-- May fail with:
--
--     * 'Kiwi.Raw.Errors.KiwiError_UnknownVariable' if the variable is not in the system.
--     * 'Kiwi.Raw.Errors.KiwiError_UnknownConstraint' if the constraint is not in the system.
--     * 'Kiwi.Raw.Errors.KiwiError_InternalSolverError' if something really sad happens internally.
removeEditVariable :: Solver -> Variable -> IO (Either KiwiError ())
removeEditVariable solverFp variable =
  withErroringCall $
    withForeignPtr solverFp $ \ solverP ->
    withForeignPtr (_variable_ptr variable) $ \ variableP ->
      kiwiSolver_removeEditVariable solverP variableP

-- |Check if a given 'Variable' is registered as an edit variable in the 'Solver'.
hasEditVariable :: Solver -> Variable -> IO Bool
hasEditVariable solverFp variable =
  withForeignPtr solverFp $ \ solverP ->
  withForeignPtr (_variable_ptr variable) $ \ variableP ->
    (/= 0) <$> kiwiSolver_hasEditVariable solverP variableP

-- |Suggest the value an edit 'Variable' should have in the constraint system.
--
-- May fail with:
--
--     * 'Kiwi.Raw.Errors.KiwiError_UnknownEditVariable' if the variable is not in the system.
--     * 'Kiwi.Raw.Errors.KiwiError_InternalSolverError' if something really sad happens internally.
suggestValue :: Solver -> Variable -> Double -> IO (Either KiwiError ())
suggestValue solverFp variable value =
  withErroringCall $
    withForeignPtr solverFp $ \ solverP ->
    withForeignPtr (_variable_ptr variable) $ \ variableP ->
      kiwiSolver_suggestValue solverP variableP value

-- |Update the 'Variable's of the constraint system by updating them to reflect the internal solver state. The resulting values can be read out using
-- 'Kiwi.Raw.Expression.getValue', 'Kiwi.Raw.Term.getValue', or 'Kiwi.Raw.Variable.getValue'.
updateVariables :: Solver -> IO ()
updateVariables solverFp =
  withForeignPtr solverFp kiwiSolver_updateVariables

-- |Reset the state of the 'Solver', removing all 'Constraint's and edit 'Variable's.
reset :: Solver -> IO ()
reset solverFp =
  withForeignPtr solverFp kiwiSolver_reset

-- |Dump the state of the 'Solver' to stdout.
dump :: Solver -> IO ()
dump solverFp =
  withForeignPtr solverFp kiwiSolver_dump

-- |Raw binding to @Solver::Solver()@
foreign import ccall unsafe kiwiSolver_new :: IO (Ptr SolverType)
-- |Raw binding to @delete@ for @Solver@.
foreign import ccall unsafe "&kiwiSolver_free" kiwiSolver_free :: FunPtr (Ptr SolverType -> IO ())
-- |Raw binding to @void Solver::addConstraint(const Constraint&)@
foreign import ccall unsafe kiwiSolver_addConstraint :: Ptr SolverType -> Ptr ConstraintType -> IO (Ptr ErrorStructType)
-- |Raw binding to @void Solver::removeConstraint(const Constraint&)@
foreign import ccall unsafe kiwiSolver_removeConstraint :: Ptr SolverType -> Ptr ConstraintType -> IO (Ptr ErrorStructType)
-- |Raw binding to @bool Solver::hasConstraint(const Constraint&)@
foreign import ccall unsafe kiwiSolver_hasConstraint :: Ptr SolverType -> Ptr ConstraintType -> IO CInt
-- |Raw binding to @void Solver::addEditVariable(const Variable&)@
foreign import ccall unsafe kiwiSolver_addEditVariable :: Ptr SolverType -> Ptr VariableType -> Double -> IO (Ptr ErrorStructType)
-- |Raw binding to @void Solver::removeEditVariable(const Variable&)@
foreign import ccall unsafe kiwiSolver_removeEditVariable :: Ptr SolverType -> Ptr VariableType -> IO (Ptr ErrorStructType)
-- |Raw binding to @bool Solver::hasEditVariable(const Variable&) const@
foreign import ccall unsafe kiwiSolver_hasEditVariable :: Ptr SolverType -> Ptr VariableType -> IO CInt
-- |Raw binding to @void Solver::suggestValue(const Variable&, double)@
foreign import ccall unsafe kiwiSolver_suggestValue :: Ptr SolverType -> Ptr VariableType -> Double -> IO (Ptr ErrorStructType)
-- |Raw binding to @void Solver::updateVariables()@
foreign import ccall unsafe kiwiSolver_updateVariables :: Ptr SolverType -> IO ()
-- |Raw binding to @void Solver::reset()@
foreign import ccall unsafe kiwiSolver_reset :: Ptr SolverType -> IO ()
-- |Raw binding to @void Solver::dump()@
foreign import ccall unsafe kiwiSolver_dump :: Ptr SolverType -> IO ()

