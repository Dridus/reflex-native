{-# LANGUAGE OverloadedStrings #-}
module ExpressionSuite (suite) where

import Kiwi
import qualified Kiwi.Raw.Constraint as RawConstraint
import qualified Kiwi.Raw.Expression as RawExpression
import qualified Kiwi.Raw.Term as RawTerm
import qualified Kiwi.Raw.Variable as RawVariable
import Prelude hiding (div)
import Test.Hspec (Spec, specify)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldMatchList, shouldReturn)


suite :: Spec
suite =
  specify "creation" $ do
    v <- variable "foo"
    v2 <- variable "bar"
    v3 <- variable "aux"
    e1 <- rawExpression $ v *. 1 +: v2 *. 2 +: v3 *. 3
    e2 <- rawExpression $ v *. 1 +: v2 *. 2 +: v3 *. 3 +: 10

    for_ [(e1, 0), (e2, 10)] $ \ (e, val) -> do
      ts <- RawExpression.getTerms e
      length ts `shouldBe` 3
      let [t1, t2, t3] = ts
      RawTerm.getVariable t1 `shouldReturn` v
      RawVariable.getCoefficient t1 `shouldReturn` 1
      RawTerm.getVariable t2 `shouldReturn` v2
      RawVariable.getCoefficient t2 `shouldReturn` 2
      RawTerm.getVariable t3 `shouldReturn` v3
      RawVariable.getCoefficient t3 `shouldReturn` 3

    show e2 `shouldBe` "foo *. 1 +: bar *. 2 +: aux *. 3 +: 10"

  specify "arithmetic operators" $ do
    v <- variable "foo"
    v2 <- variable "bar"
    let t = Term v 10
        t2 = Term v2 1
        e = t +: 5
        e2 = v2 -: 10

    let neg = negateE e
        neg_ts = _expression_terms neg

    neg_ts `shouldMatchList` [Term v (-10)]
    _expression_constant neg `shoudlBe` -5

    let mul = e *: 2
    _expression_terms mul `shouldMatchList` [Term v 20]
    _expression_constant mul `shouldBe` 10

    let div = e /: 2
    _expression_terms div `shouldMatchList` [Term v 5]
    _expression_constant div `shouldBe` 2.5

    let add = e +: 2
    _expression_terms add `shouldMatchList` [Term v 10]
    _expression_constant add `shouldBe` 7

    let add2 = e +: v2
    _expression_terms add2 `shouldMatchList` [Term v 10, Term v2 1]
    _expression_constant add2 `shouldBe` 5

    let add3 = e +: t2
    _expression_terms add3 `shouldMatchList` [Term v 10, Term v2 1]
    _expression_constant add3 `shouldBe` 5

    let add4 = e +: e2
    _expression_terms add4 `shouldMatchList` [Term v 10, Term v2 1]
    _expression_constant add4 `shouldBe` (-5)

    let sub = e -: 2
    _expression_terms sub `shouldMatchList` [Term v 10]
    _expression_constant sub `shouldBe` 3

    let sub2 = e -: v2
    _expression_terms sub2 `shouldMatchList` [Term v 10, Term v2 (-1)]
    _expression_constant sub2 `shouldBe` 5

    let sub3 = e -: t2
    _expression_terms sub3 `shouldMatchList` [Term v 10, Term v2 (-1)]
    _expression_constant sub3 `shouldBe` 5

    let sub4 = e -: e2
    _expression_terms sub4 `shouldMatchList` [Term v 10, Term v2 (-1)]
    _expression_constant sub4 `shouldBe` 15

