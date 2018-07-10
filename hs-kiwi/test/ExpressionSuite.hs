{-# LANGUAGE OverloadedStrings #-}
module ExpressionSuite (suite) where

import Data.Foldable (for_)
import Kiwi
import qualified Kiwi.Raw.Expression as RawExpression
import qualified Kiwi.Raw.Term as RawTerm
import Prelude hiding (div)
import Test.Hspec (Expectation, Spec, specify)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldMatchList, shouldReturn)


suite :: Spec
suite = do
  specify "creation" $ do
    v <- variable "foo"
    v2 <- variable "bar"
    v3 <- variable "aux"
    e1 <- rawExpression $ v *. 1 +: v2 *. 2 +: v3 *. 3
    e2 <- rawExpression $ v *. 1 +: v2 *. 2 +: v3 *. 3 +: constE 10

    for_ [(e1, 0), (e2, 10)] $ \ (e, val) -> do
      ts <- RawExpression.getTerms e
      RawExpression.getConstant e `shouldReturn` val
      length ts `shouldBe` 3
      let [t1, t2, t3] = ts
      RawTerm.getVariable t1 `shouldReturn` v
      RawTerm.getCoefficient t1 `shouldReturn` 1
      RawTerm.getVariable t2 `shouldReturn` v2
      RawTerm.getCoefficient t2 `shouldReturn` 2
      RawTerm.getVariable t3 `shouldReturn` v3
      RawTerm.getCoefficient t3 `shouldReturn` 3

  specify "arithmetic operators" $ do
    v <- variable "foo"
    v2 <- variable "bar"
    let t = Term v 10
        t2 = Term v2 1
        e = t +: constE 5
        e2 = v2 -: constE 10

    let neg = negateE e
        neg_ts = _expression_terms neg

    neg_ts `shouldMatchList` [Term v (-10)]
    _expression_constant neg `shouldBe` -5

    let mul = e *: 2
    _expression_terms mul `shouldMatchList` [Term v 20]
    _expression_constant mul `shouldBe` 10

    let div = e /: 2
    _expression_terms div `shouldMatchList` [Term v 5]
    _expression_constant div `shouldBe` 2.5

    let add = e +: constE 2
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

    let sub = e -: constE 2
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

    pure () :: Expectation

