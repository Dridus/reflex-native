{-# LANGUAGE OverloadedStrings #-}
module ConstraintSuite (suite) where

import Kiwi
import qualified Kiwi.Raw.Constraint as RawConstraint
import qualified Kiwi.Raw.Expression as RawExpression
import qualified Kiwi.Raw.Term as RawTerm
import qualified Kiwi.Raw.Variable as RawVariable
import Test.Hspec (Spec, specify)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldMatchList, shouldReturn)


suite :: Spec
suite =
  specify "creation" $ do
    v <- variable "foo"
    let c = v +: 1 ==@ 0
    rc <- rawConstraint c
    RawConstraint.getOp rc `shouldReturn` RelationalOperator_Eq
    e <- RawConstraint.getExpression rc
    RawExpression.getConstant e `shouldReturn` 1
    ts <- RawExpression.getTerms e
    length ts `shouldBe` 1
    let [t] = ts
    v <- RawTerm.getVariable t
    vn <- RawVariable.getName
    vn `shouldBe` "foo"
    coef <- RawTerm.getCoefficient t
    coef `shouldBe` 1

    show c `shouldBe` "foo *. 1 +: 1 ==@ 0"

    for_ [weak, medium, strong, required] $ \ s -> do
      rc' <- RawConstraint.new [] RelationalOperator_Ge s
      RawConstraint.getStrength rc' `shouldReturn` s

  specify "rich comparison" $ do
    v <- variable "foo"
    v2 <- variable "bar"
    let t1 = Term v 10
        e1 = t1 +: 5
        e2 = v2 -: 10

    for_ [((<=@), RelationalOperator_Le), ((==@), RelationalOperator_Eq), ((>=@), RelationalOperator_Ge)] $ \ (op, relop) -> do
      let c = e1 `op` e2
          e = _constraint_expression c
      _expression_terms e `shouldMatchList` [Term v 10, Term v2 (-1)]
      _expression_constant e `shouldBe` 15
      _constraint_operator c `shouldBe` relop

