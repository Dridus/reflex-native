{-# LANGUAGE OverloadedStrings #-}
module ConstraintSuite (suite) where

import Data.Foldable (for_)
import Kiwi.Cpp
import qualified Kiwi.Cpp.Raw.Constraint as RawConstraint
import qualified Kiwi.Cpp.Raw.Expression as RawExpression
import qualified Kiwi.Cpp.Raw.Term as RawTerm
import qualified Kiwi.Cpp.Raw.Variable as RawVariable
import Test.Hspec (Expectation, Spec, specify)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldMatchList, shouldReturn)


suite :: Spec
suite = do
  specify "creation" $ do
    v <- variable "foo"
    let c = varT v +: constE 1 ==@ constE 0
    rc <- rawConstraint required c
    RawConstraint.getOperator rc `shouldReturn` RelationalOperator_Eq
    e <- RawConstraint.getExpression rc
    RawExpression.getConstant e `shouldReturn` 1
    ts <- RawExpression.getTerms e
    length ts `shouldBe` 1
    let [t] = ts
    RawTerm.getVariable t `shouldReturn` v
    (RawVariable.getName =<< RawTerm.getVariable t) `shouldReturn` "foo"
    RawTerm.getCoefficient t `shouldReturn` 1

    show c `shouldBe` "foo *. 1.0 +: constE 1.0 ==@ 0"

  specify "rich comparison" $ do
    v <- variable "foo"
    v2 <- variable "bar"
    let t1 = Term v 10
        e1 = t1 +: constE 5
        e2 = varT v2 -: constE 10

    for_ [((<=@), RelationalOperator_Le), ((==@), RelationalOperator_Eq), ((>=@), RelationalOperator_Ge)] $ \ (op, relop) -> do
      let c = e1 `op` e2
          e = _constraint_expression c
      _expression_terms e `shouldMatchList` [Term v 10, Term v2 (-1)]
      _expression_constant e `shouldBe` 15
      _constraint_operator c `shouldBe` relop

    pure () :: Expectation

