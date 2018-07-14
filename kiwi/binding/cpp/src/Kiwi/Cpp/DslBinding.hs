{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |Binding the implementation-independent "Kiwi.Dsl" to the C++ types and functions via the @Kiwi.Cpp.Raw.*@ FFI modules.
module Kiwi.Cpp.DslBinding
  (
  -- * Monad for using a 'Solver'
    Solver, withNewSolver, withSolver, resetSolver, variable, editVariable, constrain
  , addEditVariable, hasEditVariable, removeEditVariable, suggestValue
  , addConstraint, hasConstraint, removeConstraint
  , module Kiwi.Cpp.Raw.Errors
  -- * Reading out solutions
  , updateVariables, HasValue(..)
  -- * Building raw 'Raw.Constraint's, 'Raw.Expression's and 'Raw.Term's
  , rawTerm, rawExpression, rawConstraint
  ) where

import Control.Monad ((<=<), when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import Data.Functor ((<$))
import Data.Text (Text)
import Data.Traversable (traverse)
import qualified Kiwi.Cpp.Raw.Constraint as RawConstraint
import Kiwi.Cpp.Raw.Errors (KiwiError(..))
import qualified Kiwi.Cpp.Raw.Expression as RawExpression
import qualified Kiwi.Cpp.Raw.Solver as Solver
import qualified Kiwi.Cpp.Raw.Term as RawTerm
import Kiwi.Cpp.Raw.Types (Solver, Variable)
import qualified Kiwi.Cpp.Raw.Types as Raw
import qualified Kiwi.Cpp.Raw.Variable as Variable
import Kiwi.Dsl (AsExpression, Constraint(..), Expression(..), Strength, Term(..), asExpression, required)


-- |Create a new 'Solver' and run an action with it available as environment via a 'ReaderT'.
withNewSolver :: MonadIO m => ReaderT Solver m a -> m a
withNewSolver action = do
  solver <- liftIO Solver.new
  withSolver solver action

-- |Run an action with the given 'Solver' available via a 'ReaderT'. Equivalent to @flip 'runReaderT'@.
withSolver :: Solver -> ReaderT Solver m a -> m a
withSolver = flip runReaderT

-- |Reset the 'Solver' state entirely, removing all edit variables and constraints.
resetSolver :: (MonadIO m, MonadReader Solver m) => m ()
resetSolver = ask >>= \ solver -> liftIO $ Solver.reset solver

-- |Create a new 'Variable' in any monad @m@ conforming to 'MonadIO'.
variable :: MonadIO m => Text -> m Variable
variable = liftIO . Variable.new

-- |Create a new 'Variable' and add it to the solver environment via 'addEditVariable'.
editVariable :: (MonadIO m, MonadReader Solver m) => Strength -> Text -> m Variable
editVariable str name = do
  when (str == required) . fail $ "edit variable " ++ show name ++ " cannot have required strength"
  v <- variable name
  v <$ addEditVariable str v -- note errors munched here. it's probably fine though as the variable is guaranteed fresh and we checked the strength explicitly

-- |Make a raw 'Raw.Constraint' from a 'Constraint' and add it to the solver environment with the given strength.
constrain :: (MonadIO m, MonadReader Solver m) => Strength -> Constraint Variable -> m (Either KiwiError ())
constrain str = addConstraint <=< liftIO . rawConstraint str

-- |Add a 'Variable' as an edit variable to the solver environment with the given strength.
addEditVariable :: (MonadIO m, MonadReader Solver m) => Strength -> Variable -> m (Either KiwiError ())
addEditVariable str v = ask >>= \ solver -> liftIO $ Solver.addEditVariable solver v str

-- |Check if a 'Variable' was added to the solver environment as an edit varibale.
hasEditVariable :: (MonadIO m, MonadReader Solver m) => Variable -> m Bool
hasEditVariable v = ask >>= \ solver -> liftIO $ Solver.hasEditVariable solver v

-- |Remove a previously added edit 'Variable' from the solver environment.
removeEditVariable :: (MonadIO m, MonadReader Solver m) => Variable -> m (Either KiwiError ())
removeEditVariable v = ask >>= \ solver -> liftIO $ Solver.removeEditVariable solver v

-- |Suggest a value for a previously added edit 'Variable' in the solver environment.
suggestValue :: (MonadIO m, MonadReader Solver m) => Variable -> Double -> m (Either KiwiError ())
suggestValue v d = ask >>= \ solver -> liftIO $ Solver.suggestValue solver v d

-- |Add a 'Raw.Constraint' to the constraint system of the solver.
addConstraint :: (MonadIO m, MonadReader Solver m) => Raw.Constraint -> m (Either KiwiError ())
addConstraint c = ask >>= \ solver -> liftIO $ Solver.addConstraint solver c

-- |Check if a 'Raw.Constraint' is in the constraint system of the solver.
hasConstraint :: (MonadIO m, MonadReader Solver m) => Raw.Constraint -> m Bool
hasConstraint c = ask >>= \ solver -> liftIO $ Solver.hasConstraint solver c

-- |Remove a 'Raw.Constraint' from the constraint system of the solver.
removeConstraint :: (MonadIO m, MonadReader Solver m) => Raw.Constraint -> m (Either KiwiError ())
removeConstraint c = ask >>= \ solver -> liftIO $ Solver.removeConstraint solver c

-- |Update the values visible 'Variable's in the solver environment so they can be read. Using 'getValue' prior to executing this action will return whatever
-- the previous value was, not the current and accurate one.
updateVariables :: (MonadIO m, MonadReader Solver m) => m ()
updateVariables = ask >>= \ solver -> liftIO $ Solver.updateVariables solver

-- |Class of things which have a value that can be read out: 'Variable's, 'Raw.Term's, and 'Raw.Expression's.
class HasValue a where
  -- |Read out the last updated value of the entity.
  getValue :: MonadIO m => a -> m Double

-- |Read the value of a 'Variable'.
instance HasValue Variable where
  getValue = liftIO . Variable.getValue

-- |Read the value of a 'Raw.Term'.
instance HasValue Raw.Term where
  getValue = liftIO . RawTerm.getValue

-- |Read the value of a 'Raw.Expression'.
instance HasValue Raw.Expression where
  getValue = liftIO . RawExpression.getValue

-- |Build a 'Raw.Term' from a 'Term'.
rawTerm :: MonadIO m => Term Variable -> m Raw.Term
rawTerm (Term v f) = liftIO $ RawTerm.new v f

-- |Build an 'Raw.Expression' from EDSL description, which can be an 'Expression', 'Term', 'Variable', or 'Double' by way of 'AsExpression'.
rawExpression :: (AsExpression Variable a, MonadIO m) => a -> m Raw.Expression
rawExpression a = do
  let Expression ts c = asExpression a
  rawTerms <- traverse rawTerm ts
  liftIO $ RawExpression.new rawTerms c

-- |Build a 'Raw.Constraint' from a 'Constraint'.
rawConstraint :: MonadIO m => Strength -> Constraint Variable -> m Raw.Constraint
rawConstraint str (Constraint expr op) = do
  rawExpr <- rawExpression expr
  liftIO $ RawConstraint.new rawExpr op str

