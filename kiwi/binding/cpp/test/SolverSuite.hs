{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SolverSuite (suite) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Kiwi.Cpp
import qualified Kiwi.Cpp.Raw.Constraint as RawConstraint
import qualified Kiwi.Cpp.Raw.Expression as RawExpression
import qualified Kiwi.Cpp.Raw.Term as RawTerm
import Test.Hspec (Expectation, Spec, specify)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldReturn)


suite :: Spec
suite = do
  specify "creation" $ do
    withNewSolver $ ask >>= \ (_ :: Solver) -> pure ()
    pure () :: Expectation

  specify "managing edit variables" . withNewSolver $ do
    v1 <- variable "foo"
    v2 <- variable "bar"

    hasEditVariable v1 `shouldReturn` False
    addEditVariable weak v1 `shouldReturn` Right ()
    hasEditVariable v1 `shouldReturn` True
    addEditVariable medium v1 `shouldReturn` Left KiwiError_DuplicateEditVariable
    removeEditVariable v2 `shouldReturn` Left KiwiError_UnknownEditVariable
    removeEditVariable v1 `shouldReturn` Right ()
    hasEditVariable v1 `shouldReturn` False

    addEditVariable required v1 `shouldReturn` Left KiwiError_BadRequiredStrength

    addEditVariable strong v2 `shouldReturn` Right ()
    hasEditVariable v2 `shouldReturn` True
    suggestValue v1 10 `shouldReturn` Left KiwiError_UnknownEditVariable

    lift (pure () :: Expectation)

  specify "managing constraints" . withNewSolver $ do
    v <- variable "foo"
    rc1 <- rawConstraint required $ varT v >=@ constE 1
    rc2 <- rawConstraint required $ varT v <=@ constE 0

    hasConstraint rc1 `shouldReturn` False
    addConstraint rc1 `shouldReturn` Right ()
    hasConstraint rc1 `shouldReturn` True
    addConstraint rc1 `shouldReturn` Left KiwiError_DuplicateConstraint
    removeConstraint rc2 `shouldReturn` Left KiwiError_UnknownConstraint
    addConstraint rc2 `shouldReturn` Left KiwiError_UnsatisfiableConstraint
    hasConstraint rc2 `shouldReturn` False
    removeConstraint rc1 `shouldReturn` Right ()
    hasConstraint rc1 `shouldReturn` False

    addConstraint rc2 `shouldReturn` Right ()
    hasConstraint rc2 `shouldReturn` True
    resetSolver
    hasConstraint rc2 `shouldReturn` False

    lift (pure () :: Expectation)

  specify "solving an under constrained system" . withNewSolver $ do
    v <- variable "foo"
    rc <- rawConstraint required $ varT v *. 2 +: constE 1 >=@ constE 0

    addEditVariable weak v `shouldReturn` Right ()
    addConstraint rc `shouldReturn` Right ()
    suggestValue v 10 `shouldReturn` Right ()
    updateVariables

    e <- liftIO $ RawConstraint.getExpression rc
    getValue e `shouldReturn` 21
    ts <- liftIO $ RawExpression.getTerms e
    length ts `shouldBe` 1
    let [t] = ts
    getValue t `shouldReturn` 20
    (getValue =<< liftIO (RawTerm.getVariable t)) `shouldReturn` 10

    lift (pure () :: Expectation)

  specify "solving with strength" . withNewSolver $ do
    v1 <- variable "foo"
    v2 <- variable "bar"

    constrain required (varT v1 +: varT v2 ==@ constE 0) `shouldReturn` Right ()
    constrain required (varT v1 ==@ constE 10) `shouldReturn` Right ()
    constrain weak (varT v2 >=@ constE 0) `shouldReturn` Right ()
    updateVariables
    getValue v1 `shouldReturn` 10
    getValue v2 `shouldReturn` -10

    resetSolver

    constrain required (varT v1 +: varT v2 ==@ constE 0) `shouldReturn` Right ()
    constrain medium (varT v1 >=@ constE 10) `shouldReturn` Right ()
    constrain strong (varT v2 ==@ constE 2) `shouldReturn` Right ()
    updateVariables
    getValue v1 `shouldReturn` (-2)
    getValue v2 `shouldReturn` 2

    lift (pure () :: Expectation)

