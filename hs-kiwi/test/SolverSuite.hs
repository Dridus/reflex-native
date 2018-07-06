{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SolverSuite (suite) where

import Kiwi
import qualified Kiwi.Raw.Constraint as RawConstraint
import qualified Kiwi.Raw.Expression as RawExpression
import qualified Kiwi.Raw.Term as RawTerm
import qualified Kiwi.Raw.Variable as RawVariable
import Test.Hspec (Spec, specify)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldReturn)


suite :: Spec
suite = do
  specify "creation" $ do
    withNewSolver $ ask >>= \ (_ :: Solver) -> pure ()

  specify "managing edit variables" $ do
    withNewSolver $ do
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
      suggestValue v2 10 `shouldReturn` Left KiwiError_UnknownEditVariable

  specify "managing constraints" $ do
    withNewSolver $ do
      v <- variable "foo"
      rc1 <- rawConstraint $ v >=@ 1
      rc2 <- rawConstraint $ v <=@ 0

      hasConstraint rc1 `shouldReturn` False
      addConstraint required rc1 `shouldReturn` Right ()
      hasConstraint rc1 `shouldReturn` True
      addConstraint required rc1 `shouldReturn` Left KiwiError_DuplicateConstraint
      removeConstraint rc2 `shouldReturn` Left KiwiError_UnknownConstraint
      addConstraint required rc2 `shouldReturn` Left KiwiError_UnsatisfiableConstraint
      hasConstraint rc2 `shouldReturn` False
      removeConstraint rc1 `shouldReturn` Right ()
      hasConstraint rc1 `shouldReturn` False

      addConstraint required rc2 `shouldReturn` Right ()
      hasConstraint rc2 `shouldReturn` True
      resetSolver
      hasConstraint rc2 `shouldReturn` False

  specify "solving an under constrained system" $ do
    withNewSolver $ do
      v <- variable "foo"
      rc <- rawConstraint $ v *. 2 +: 1 >=@ 0

      addEditVariable weak v
      addConstraint required rc
      suggestValue v 10
      updateVariables

      getValue rc `shouldReturn` 21
      e <- liftIO $ RawConstraint.getExpression rc
      ts <- liftIO $ RawExpression.getTerms e
      length ts `shouldBe` 1
      let [t] = ts
      getValue t `shouldReturn` 20
      v <- liftIO $ RawTerm.getVariable t
      getValue v `shouldReturn` 10

  specify "solving with strength" $ do
    withNewSolver $ do
      v1 <- variable "foo"
      v2 <- variable "bar"

      constrain required $ v1 +: v2 ==@ 0
      constrain required $ v1 ==@ 10
      constrain weak $ v2 >=@ 0
      updateVariables
      getValue v1 `shouldReturn` 10
      getValue v2 `shouldReturn` -10

      resetSolver

      constrain required $ v1 +: v2 ==@ 0
      constrain medium $ v1 >=@ 10
      constrain strong $ v2 ==@ 2
      updateVariables
      getValue v1 `shouldReturn` (-2)
      getValue v2 `shouldReturn` 2

