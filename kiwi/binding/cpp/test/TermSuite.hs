{-# LANGUAGE OverloadedStrings #-}
module TermSuite (suite) where

import Data.Foldable (for_)
import Kiwi.Cpp
import qualified Kiwi.Cpp.Raw.Term as RawTerm
import Test.Hspec (Expectation, Spec, specify)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldMatchList, shouldReturn)


suite :: Spec
suite = do
  specify "creation" $ do
    v <- variable "foo"
    rt <- RawTerm.new v 1
    RawTerm.getVariable rt `shouldReturn` v
    RawTerm.getCoefficient rt `shouldReturn` 1

    rt2 <- RawTerm.new v 100
    RawTerm.getVariable rt2 `shouldReturn` v
    RawTerm.getCoefficient rt2 `shouldReturn` 100

  specify "arithmetic operators" $ do
    v <- variable "foo"
    v2 <- variable "bar"
    let t = varT v *. 10
        t2 = varT v2

    negateT t `shouldBe` Term v (-10)
    t *. 2 `shouldBe` Term v 20
    t /. 2 `shouldBe` Term v 5
    t +: constE 2 `shouldBe` Expression [Term v 10] 2
    t +: varT v2 `shouldBe` Expression [Term v 10, Term v2 1] 0
    t +: t2 `shouldBe` Expression [Term v 10, Term v2 1] 0
    t -: constE 2 `shouldBe` Expression [Term v 10] (-2)
    t -: varT v2 `shouldBe` Expression [Term v 10, Term v2 (-1)] 0
    t -: t2 `shouldBe` Expression [Term v 10, Term v2 (-1)] 0

    pure () :: Expectation

  specify "constraint operators" $ do
    v <- variable "foo"
    v2 <- variable "bar"
    let t1 = varT v *. 10
        t2 = varT v2 *. 20

    for_ [((<=@), RelationalOperator_Le), ((==@), RelationalOperator_Eq), ((>=@), RelationalOperator_Ge)] $ \ (op, relop) -> do
      let c = t1 `op` (t2 +: constE 1)
          e = _constraint_expression c
      _expression_terms e `shouldMatchList` [Term v 10, Term v2 (-20)]
      _expression_constant e `shouldBe` (-1)
      _constraint_operator c `shouldBe` relop

    pure () :: Expectation
