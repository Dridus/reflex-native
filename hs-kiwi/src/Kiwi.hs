{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |'Expression' and 'Constraint' builder EDSL providing a thin syntax layer on top of the underlying "Kiwi.Raw.Constraint", "Kiwi.Raw.Expression",
-- "Kiwi.Raw.Term", and "Kiwi.Raw.Variable" calls.
--
-- For example (from the Cassowary technical report):
--
-- @
--     import Kiwi
--
--     main :: IO ()
--     main = 'withNewSolver' $ do
--       xl <- 'variable' "xl"
--       xm <- 'editVariable' strong "xm"
--       xr <- 'variable' "xr"
--       'constrain' 'required' $ xm '*.' 2 '==@' xl '+:' xr
--       'constrain' 'required' $ xl '+:' 10 '<=@' xr
--       'constrain' 'required' $ xr '<=@' 100
--       'constrain' 'required' $ 0 '<=@' xl
--       'updateVariables'
--       liftIO . print =<< 'getValue' xl
--       liftIO . print =<< 'getValue' xm
--       liftIO . print =<< 'getValue' xr
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
module Kiwi
  (
  -- * Variables
    Variable, variable
  -- * Terms of an expression
  -- ** 'Term' and implicit promotion
  , Term(..), AsTerm(..)
  -- ** Operators to construct terms
  , (*.), mulT, (/.), divT, negateT
  -- * Expressions
  -- ** 'Expression' and implicit promotion
  , Expression(..), AsExpression(..)
  -- ** Operators to construct expressions
  , (*:), mulE, (/:), divE, (+:), addE, (-:), subE, negateE, constE
  -- * Constraints
  , Constraint(..), RelationalOperator(..), (==@), eqC, (<=@), leC, (>=@), geC
  , module Kiwi.Raw.Strength
  -- * Monad for using a 'Solver'
  , Solver, withNewSolver, withSolver, resetSolver, editVariable, constrain
  , addEditVariable, hasEditVariable, removeEditVariable, suggestValue
  , addConstraint, hasConstraint, removeConstraint
  , module Kiwi.Raw.Errors
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
import GHC.Generics (Generic)
import Kiwi.Raw.Constraint (RelationalOperator(..))
import qualified Kiwi.Raw.Constraint as RawConstraint
import Kiwi.Raw.Errors (KiwiError(..))
import qualified Kiwi.Raw.Expression as RawExpression
import qualified Kiwi.Raw.Solver as Solver
import Kiwi.Raw.Strength (Strength, required, strong, medium, weak, mkStrength, mkStrengthWeighted)
import qualified Kiwi.Raw.Term as RawTerm
import Kiwi.Raw.Types (Solver, Variable)
import qualified Kiwi.Raw.Types as Raw
import qualified Kiwi.Raw.Variable as Variable


-- |Create a new 'Variable' in any monad @m@ conforming to 'MonadIO'.
variable :: MonadIO m => Text -> m Variable
variable = liftIO . Variable.new

-- |A single term in a linear expression 'Expression' of the form @v * f@ where @v@ is a 'Variable' and @f@ is a Double coefficient. Constructed implicitly by
-- promoting a 'Variable' by way of 'AsTerm', or using '(*.)' ('mulT') or '(/.)' ('divT').
data Term = Term
  { _exprTerm_variable :: Variable
  -- ^The variable in the term (@v@ in @v * f@)
  , _exprTerm_coefficient :: Double
  -- ^The coefficient in the term (@f@ in @v * f@)
  } deriving (Eq, Generic)

instance Show Term where
  showsPrec d (Term v f) = showParen (d > 7) $ showsPrec 8 v . showString " *. " . showsPrec 8 f

-- |Class of types which can be converted to 'Term': 'Term's (trivially), or 'Variable's.
class AsTerm a where
  asTerm :: a -> Term

-- |'Term's are trivially 'Term's
instance AsTerm Term where
  asTerm = id

-- |'Variable's can be 'Term's by using the unit coefficient, i.e. @v*1@
instance AsTerm Variable where
  asTerm v = Term v 1.0

infixl 7 *., /.

-- |Multiply a 'Variable' or 'Term' by a coefficient to make an 'Term'. Equivalent to '(*.)'.
--
-- Types which this function conforms to by way of 'AsTerm':
--
-- @
--     mulT :: Term     -> Double -> Term
--     mulT :: Variable -> Double -> Term
-- @
mulT :: AsTerm a => a -> Double -> Term
mulT = (*.)

-- |Multiply a 'Variable' or 'Term' by a coefficient to make an 'Term'. Equivalent to 'mulT'.
--
-- Types which this function conforms to by way of 'AsTerm':
--
-- @
--     (*.) :: Term     -> Double -> Term
--     (*.) :: Variable -> Double -> Term
-- @
(*.) :: AsTerm a => a -> Double -> Term
a *. f =
  let Term v f' = asTerm a
  in Term v (f*f')

-- |Divide a 'Variable' or 'Term' by a coefficient to make an 'Term'. Equivalent to '(/.)' or @'(*.)' . (1/)@.
--
-- Types which this function conforms to by way of 'AsTerm':
--
-- @
--     divT :: Term     -> Double -> Term
--     divT :: Variable -> Double -> Term
-- @
divT :: AsTerm a => a -> Double -> Term
divT = (/.)

-- |Divide a 'Variable' or 'Term' by a coefficient to make an 'Term'. Equivalent to 'divT' or @'(*.)' . (1/)@.
--
-- Types which this function conforms to by way of 'AsTerm':
--
-- @
--     (/.) :: Term     -> Double -> Term
--     (/.) :: Variable -> Double -> Term
-- @
(/.) :: AsTerm a => a -> Double -> Term
a /. f = a *. (1/f)

-- |Negate a 'Variable' or 'Term' by multiplying it by @-1@.
--
-- @
--     negateT :: Term     -> Term
--     negateT :: Variable -> Term
-- @
negateT :: AsTerm a => a -> Term
negateT a = a *. (-1)

-- |A linear expression of the form @v1*f1 + v2*f2 + … + c@ where @vN@ are 'Variable's, @fN@ are coefficients, and @c@ is a constant. Constructed implicitly
-- by promoting a 'Double', 'Variable', or 'Term' by way of 'AsExpression', or using '(*:)' ('mulE'), '(/:)' ('divE'), '(+:)' ('addE'), or '(-:)' ('subE').
data Expression = Expression
  { _expression_terms :: [Term]
  -- ^The terms of the expression (@vN*fN@ in @v1*f1 + v2*f2 + … + c@)
  , _expression_constant :: Double
  -- ^The constant of the expression (@c@ in @v1*f1 + v2*f2 + … + c@)
  } deriving (Eq, Generic)

instance Show Expression where
  showsPrec d (Expression ts c) =
    showParen (d > 6)
      $ foldr (\ t r -> showsPrec 7 t . showString " +: " . r) id ts
      . showString "constE "
      . showsPrec 7 c

-- |Class of things ('Expression's and 'Term's) which can be trivially converted to an 'Expression' for building with 'expression'.
class AsExpression a where
  asExpression :: a -> Expression

-- |'Expression's are trivially 'Expression's
instance AsExpression Expression where
  asExpression = id

-- |'Term's can be 'Expression's by adding 0, i.e. @t + 0@
instance AsExpression Term where
  asExpression t = Expression [t] 0

-- |'Variable's can be 'Expression's by using the unit coefficient and adding 0, i.e. @v*1 + 0@
instance AsExpression Variable where
  asExpression = asExpression . asTerm

-- |Doubles can be 'Expression's by using that as the constant and having no terms.
instance AsExpression Double where
  asExpression = Expression []

infixl 7 *:, /:
infixl 6 +:, -:

-- |Multiply an 'Expression', 'Term', 'Variable', or 'Double' by a scalar multiple to make an 'Expression'. Equivalent to '(*:)'.
-- Promotes the left hand side to an 'Expression' using 'AsExpression' then multiplies each term's coefficient and the constant by the given scalar.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     mulE :: Expression -> Double -> Expression
--     mulE :: Term       -> Double -> Expression
--     mulE :: Variable   -> Double -> Expression
-- @
mulE :: AsExpression a => a -> Double -> Expression
mulE = (*:)

-- |Multiply an 'Expression', 'Term', 'Variable', or 'Double' by a scalar multiple to make an 'Expression'. Equivalent to 'mulE'.
-- Promotes the left hand side to an 'Expression' using 'AsExpression' then multiplies each term's coefficient and the constant by the given scalar.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (*:) :: Expression -> Double -> Expression
--     (*:) :: Term       -> Double -> Expression
--     (*:) :: Variable   -> Double -> Expression
-- @
(*:) :: AsExpression a => a -> Double -> Expression
a *: s =
  let Expression ts c = asExpression a
  in Expression (map (*. s) ts) (c * s)

-- |Divide an 'Expression', 'Term', 'Variable', or 'Double' by a scalar divisor to make an 'Expression'. Equivalent to '(/:)'.
-- Promotes the left hand side to an 'Expression' using 'AsExpression' then multiplies each term's coefficient and the constant by the reciprocal.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     divE :: Expression -> Double -> Expression
--     divE :: Term       -> Double -> Expression
--     divE :: Variable   -> Double -> Expression
-- @
divE :: AsExpression a => a -> Double -> Expression
divE = (/:)

-- |Divide an 'Expression', 'Term', 'Variable', or 'Double' by a scalar divisor to make an 'Expression'. Equivalent to 'divE'.
-- Promotes the left hand side to an 'Expression' using 'AsExpression' then multiplies each term's coefficient and the constant by the reciprocal.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (/:) :: Expression -> Double -> Expression
--     (/:) :: Term       -> Double -> Expression
--     (/:) :: Variable   -> Double -> Expression
-- @
(/:) :: AsExpression a => a -> Double -> Expression
a /: s =
  let Expression ts c = asExpression a
  in Expression (map (/. s) ts) (c / s)


-- |Add an 'Expression', 'Term', 'Variable', or 'Double' to another 'Expression', 'Term', 'Variable', or 'Double' to make an 'Expression'. Equivalent to '(+:)'.
-- Promotes both sides to 'Expression's using 'AsExpression' then concatenates the terms and adds the constants of each to form the result.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     addE :: Expression -> Term     -> Expression
--     addE :: Term       -> Term     -> Expression
--     addE :: Variable   -> Term     -> Expression
--     addE :: Double     -> Term     -> Expression
--     addE :: Expression -> Variable -> Expression
--     addE :: Term       -> Variable -> Expression
--     addE :: Variable   -> Variable -> Expression
--     addE :: Double     -> Variable -> Expression
--     addE :: Expression -> Double   -> Expression
--     addE :: Term       -> Double   -> Expression
--     addE :: Variable   -> Double   -> Expression
--     addE :: Double     -> Double   -> Expression
-- @
addE :: (AsExpression a, AsExpression b) => a -> b -> Expression
addE = (+:)

-- |Add an 'Expression', 'Term', 'Variable', or 'Double' to another 'Expression', 'Term', 'Variable', or 'Double' to make an 'Expression'. Equivalent to 'addE'.
-- Promotes both sides to 'Expression's using 'AsExpression' then concatenates the terms and adds the constants of each to form the result.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (+:) :: Expression -> Term     -> Expression
--     (+:) :: Term       -> Term     -> Expression
--     (+:) :: Variable   -> Term     -> Expression
--     (+:) :: Double     -> Term     -> Expression
--     (+:) :: Expression -> Variable -> Expression
--     (+:) :: Term       -> Variable -> Expression
--     (+:) :: Variable   -> Variable -> Expression
--     (+:) :: Double     -> Variable -> Expression
--     (+:) :: Expression -> Double   -> Expression
--     (+:) :: Term       -> Double   -> Expression
--     (+:) :: Variable   -> Double   -> Expression
--     (+:) :: Double     -> Double   -> Expression
-- @
(+:) :: (AsExpression a, AsExpression b) => a -> b -> Expression
a +: b =
  let Expression tsa ca = asExpression a
      Expression tsb cb = asExpression b
  in Expression (tsa ++ tsb) (ca + cb)

-- |Subtract an 'Expression', 'Term', 'Variable', or 'Double' from another 'Expression', 'Term', 'Variable', or 'Double' to make an 'Expression'. Equivalent to '(-:)'.
-- Promotes both sides to 'Expression's using 'AsExpression' then adds ('(+:)' / 'addE') the left to the negated ('negateE') right.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     subE :: Expression -> Term     -> Expression
--     subE :: Term       -> Term     -> Expression
--     subE :: Variable   -> Term     -> Expression
--     subE :: Double     -> Term     -> Expression
--     subE :: Expression -> Variable -> Expression
--     subE :: Term       -> Variable -> Expression
--     subE :: Variable   -> Variable -> Expression
--     subE :: Double     -> Variable -> Expression
--     subE :: Expression -> Double   -> Expression
--     subE :: Term       -> Double   -> Expression
--     subE :: Variable   -> Double   -> Expression
--     subE :: Double     -> Double   -> Expression
-- @
subE :: (AsExpression a, AsExpression b) => a -> b -> Expression
subE = (-:)

-- |Subtract an 'Expression', 'Term', 'Variable', or 'Double' from another 'Expression', 'Term', 'Variable', or 'Double' to make an 'Expression'. Equivalent to 'subE'.
-- Promotes both sides to 'Expression's using 'AsExpression' then adds ('(+:)' / 'addE') the left to the negated ('negateE') right.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (-:) :: Expression -> Term     -> Expression
--     (-:) :: Term       -> Term     -> Expression
--     (-:) :: Variable   -> Term     -> Expression
--     (-:) :: Double     -> Term     -> Expression
--     (-:) :: Expression -> Variable -> Expression
--     (-:) :: Term       -> Variable -> Expression
--     (-:) :: Variable   -> Variable -> Expression
--     (-:) :: Double     -> Variable -> Expression
--     (-:) :: Expression -> Double   -> Expression
--     (-:) :: Term       -> Double   -> Expression
--     (-:) :: Variable   -> Double   -> Expression
--     (-:) :: Double     -> Double   -> Expression
-- @
(-:) :: (AsExpression a, AsExpression b) => a -> b -> Expression
a -: b = a +: negateE b

-- |Negate an 'Expression', 'Term', 'Variable', or 'Double' to produce an 'Expression'. Promotes the value to 'Expression' by way of 'AsExpression' and then multiplies with @-1@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     negateE :: Expression     -> Expression
--     negateE :: Term -> Expression
--     negateE :: Variable -> Expression
-- @
negateE :: AsExpression a => a -> Expression
negateE = (*: (-1)) . asExpression

-- |Convert a literal 'Double' into an 'Expression'. Helper to fix types where they would be ambiguous, usually when adding a constant to an 'Expression'.
constE :: Double -> Expression
constE = asExpression

-- |A constraint that can be added to the constraint system and optimized, consisting of some 'Expression' and its intended relation to 0. The strength of
-- the constraint is established when it's converted into a 'Raw.Constraint'.
data Constraint = Constraint
  { _constraint_expression :: Expression
  -- ^The expression of the constraint, describing the intended relationship to satisfy.
  , _constraint_operator :: RelationalOperator
  -- ^The relation of the expression to 0.
  } deriving (Eq, Generic)

instance Show Constraint where
  showsPrec d (Constraint e o) =
    showParen (d > 4)
      $ showsPrec 5 e
      . showString (case o of { RelationalOperator_Eq -> " ==@ 0"; RelationalOperator_Le -> " <=@ 0"; RelationalOperator_Ge -> " >=@ 0" })

infixl 4 ==@, <=@, >=@

-- |Create a constraint of equality between two 'Expression's, 'Term's, 'Variable's, or 'Double's. Equivalent to '(==@)'.
-- Promotes both sides via 'AsExpression' then creates a constraint that @left - right == 0@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     eqC :: Expression -> Term     -> Expression
--     eqC :: Term       -> Term     -> Expression
--     eqC :: Variable   -> Term     -> Expression
--     eqC :: Double     -> Term     -> Expression
--     eqC :: Expression -> Variable -> Expression
--     eqC :: Term       -> Variable -> Expression
--     eqC :: Variable   -> Variable -> Expression
--     eqC :: Double     -> Variable -> Expression
--     eqC :: Expression -> Double   -> Expression
--     eqC :: Term       -> Double   -> Expression
--     eqC :: Variable   -> Double   -> Expression
--     eqC :: Double     -> Double   -> Expression
-- @
eqC :: (AsExpression a, AsExpression b) => a -> b -> Constraint
eqC = (==@)

-- |Create a constraint of equality between two 'Expression's, 'Term's, 'Variable's, or 'Double's. Equivalent to 'eqC'.
-- Promotes both sides via 'AsExpression' then creates a constraint that @left - right == 0@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (==@) :: Expression -> Term     -> Expression
--     (==@) :: Term       -> Term     -> Expression
--     (==@) :: Variable   -> Term     -> Expression
--     (==@) :: Double     -> Term     -> Expression
--     (==@) :: Expression -> Variable -> Expression
--     (==@) :: Term       -> Variable -> Expression
--     (==@) :: Variable   -> Variable -> Expression
--     (==@) :: Double     -> Variable -> Expression
--     (==@) :: Expression -> Double   -> Expression
--     (==@) :: Term       -> Double   -> Expression
--     (==@) :: Variable   -> Double   -> Expression
--     (==@) :: Double     -> Double   -> Expression
-- @
(==@) :: (AsExpression a, AsExpression b) => a -> b -> Constraint
a ==@ b = Constraint (a -: b) RelationalOperator_Eq

-- |Create a constraint of equality between two 'Expression's, 'Term's, 'Variable's, or 'Double's. Equivalent to '(<=@)'.
-- Promotes both sides via 'AsExpression' then creates a constraint that @left - right <= 0@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     leC :: Expression -> Term     -> Expression
--     leC :: Term       -> Term     -> Expression
--     leC :: Variable   -> Term     -> Expression
--     leC :: Double     -> Term     -> Expression
--     leC :: Expression -> Variable -> Expression
--     leC :: Term       -> Variable -> Expression
--     leC :: Variable   -> Variable -> Expression
--     leC :: Double     -> Variable -> Expression
--     leC :: Expression -> Double   -> Expression
--     leC :: Term       -> Double   -> Expression
--     leC :: Variable   -> Double   -> Expression
--     leC :: Double     -> Double   -> Expression
-- @
leC :: (AsExpression a, AsExpression b) => a -> b -> Constraint
leC = (<=@)

-- |Create a constraint of equality between two 'Expression's, 'Term's, 'Variable's, or 'Double's. Equivalent to 'leC'.
-- Promotes both sides via 'AsExpression' then creates a constraint that @left - right <= 0@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (<=@) :: Expression -> Term     -> Expression
--     (<=@) :: Term       -> Term     -> Expression
--     (<=@) :: Variable   -> Term     -> Expression
--     (<=@) :: Double     -> Term     -> Expression
--     (<=@) :: Expression -> Variable -> Expression
--     (<=@) :: Term       -> Variable -> Expression
--     (<=@) :: Variable   -> Variable -> Expression
--     (<=@) :: Double     -> Variable -> Expression
--     (<=@) :: Expression -> Double   -> Expression
--     (<=@) :: Term       -> Double   -> Expression
--     (<=@) :: Variable   -> Double   -> Expression
--     (<=@) :: Double     -> Double   -> Expression
-- @
(<=@) :: (AsExpression a, AsExpression b) => a -> b -> Constraint
a <=@ b = Constraint (a -: b) RelationalOperator_Le

-- |Create a constraint of equality between two 'Expression's, 'Term's, 'Variable's, or 'Double's. Equivalent to '(>=@)'.
-- Promotes both sides via 'AsExpression' then creates a constraint that @left - right >= 0@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     geC :: Expression -> Term     -> Expression
--     geC :: Term       -> Term     -> Expression
--     geC :: Variable   -> Term     -> Expression
--     geC :: Double     -> Term     -> Expression
--     geC :: Expression -> Variable -> Expression
--     geC :: Term       -> Variable -> Expression
--     geC :: Variable   -> Variable -> Expression
--     geC :: Double     -> Variable -> Expression
--     geC :: Expression -> Double   -> Expression
--     geC :: Term       -> Double   -> Expression
--     geC :: Variable   -> Double   -> Expression
--     geC :: Double     -> Double   -> Expression
-- @
geC :: (AsExpression a, AsExpression b) => a -> b -> Constraint
geC = (>=@)

-- |Create a constraint of equality between two 'Expression's, 'Term's, 'Variable's, or 'Double's. Equivalent to 'geC'.
-- Promotes both sides via 'AsExpression' then creates a constraint that @left - right >= 0@.
--
-- Types which this function conforms to by way of 'AsExpression':
--
-- @
--     (>=@) :: Expression -> Term     -> Expression
--     (>=@) :: Term       -> Term     -> Expression
--     (>=@) :: Variable   -> Term     -> Expression
--     (>=@) :: Double     -> Term     -> Expression
--     (>=@) :: Expression -> Variable -> Expression
--     (>=@) :: Term       -> Variable -> Expression
--     (>=@) :: Variable   -> Variable -> Expression
--     (>=@) :: Double     -> Variable -> Expression
--     (>=@) :: Expression -> Double   -> Expression
--     (>=@) :: Term       -> Double   -> Expression
--     (>=@) :: Variable   -> Double   -> Expression
--     (>=@) :: Double     -> Double   -> Expression
-- @
(>=@) :: (AsExpression a, AsExpression b) => a -> b -> Constraint
a >=@ b = Constraint (a -: b) RelationalOperator_Ge

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

-- |Create a new 'Variable' and add it to the solver environment via 'addEditVariable'.
editVariable :: (MonadIO m, MonadReader Solver m) => Strength -> Text -> m Variable
editVariable str name = do
  when (str == required) . fail $ "edit variable " ++ show name ++ " cannot have required strength"
  v <- variable name
  v <$ addEditVariable str v -- note errors munched here. it's probably fine though as the variable is guaranteed fresh and we checked the strength explicitly

-- |Make a raw 'Raw.Constraint' from a 'Constraint' and add it to the solver environment with the given strength.
constrain :: (MonadIO m, MonadReader Solver m) => Strength -> Constraint -> m (Either KiwiError ())
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

-- |Build a 'Raw.Term' from a 'Term' or 'Variable' by way of 'AsTerm'.
rawTerm :: (AsTerm a, MonadIO m) => a -> m Raw.Term
rawTerm a =
  let Term v f = asTerm a
  in liftIO $ RawTerm.new v f

-- |Build an 'Raw.Expression' from EDSL description, which can be an 'Expression', 'Term', 'Variable', or 'Double' by way of 'AsExpression'.
rawExpression :: (AsExpression a, MonadIO m) => a -> m Raw.Expression
rawExpression a = do
  let Expression ts c = asExpression a
  rawTerms <- traverse rawTerm ts
  liftIO $ RawExpression.new rawTerms c

-- |Build a 'Raw.Constraint' from a 'Constraint'.
rawConstraint :: MonadIO m => Strength -> Constraint -> m Raw.Constraint
rawConstraint str (Constraint expr op) = do
  rawExpr <- rawExpression expr
  liftIO $ RawConstraint.new rawExpr op str

