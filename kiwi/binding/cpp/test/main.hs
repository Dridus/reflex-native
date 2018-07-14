module Main where

import qualified ConstraintSuite
import qualified ExpressionSuite
import qualified ReadmeSuite
import qualified SolverSuite
import qualified StrengthSuite
import qualified TermSuite
import qualified VariableSuite
import Test.Hspec (hspec, describe)


main :: IO ()
main = hspec $ do
  describe "Constraint" ConstraintSuite.suite
  describe "Expression" ExpressionSuite.suite
  describe "Readme" ReadmeSuite.suite
  describe "Solver" SolverSuite.suite
  describe "Strength" StrengthSuite.suite
  describe "Term" TermSuite.suite
  describe "Variable" VariableSuite.suite

