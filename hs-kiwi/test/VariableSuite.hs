{-# LANGUAGE OverloadedStrings #-}
module VariableSuite (suite) where

import Data.Foldable (for_)
import Kiwi
import Kiwi.Raw.Types (Variable(..))
import qualified Kiwi.Raw.Variable as RawVariable
import Test.Hspec (Expectation, Spec, specify)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldMatchList, shouldReturn, shouldStartWith)


suite :: Spec
suite = do
  specify "creation" $ do
    v <- variable ""
    RawVariable.getName v `shouldReturn` ""
    _variable_name v `shouldBe` ""
    RawVariable.getValue v `shouldReturn` 0
    show v `shouldStartWith` "variable 0x"

    v2 <- variable "γ"
    RawVariable.getName v2 `shouldReturn` "γ"
    _variable_name v2 `shouldBe` "γ"
    show v2 `shouldBe` "γ"

  specify "arithmetic operators" $ do
    v <- variable "foo"
    v2 <- variable "bar"

    negateT v `shouldBe` Term v (-1)
    v *. 2 `shouldBe` Term v 2
    v /. 2 `shouldBe` Term v 0.5
    v +: constE 2 `shouldBe` Expression [Term v 1] 2
    v +: v2 `shouldBe` Expression [Term v 1, Term v2 1] 0
    v -: constE 2 `shouldBe` Expression [Term v 1] (-2)
    v -: v2 `shouldBe` Expression [Term v 1, Term v2 (-1)] 0

    pure () :: Expectation

  specify "constraint operators" $ do
    v <- variable "foo"
    v2 <- variable "bar"

    for_ [((<=@), RelationalOperator_Le), ((==@), RelationalOperator_Eq), ((>=@), RelationalOperator_Ge)] $ \ (op, relop) -> do
      let c = v `op` (v2 +: constE 1)
          e = _constraint_expression c
      _expression_terms e `shouldMatchList` [Term v 1, Term v2 (-1)]
      _expression_constant e `shouldBe` (-1)
      _constraint_operator c `shouldBe` relop

    pure () :: Expectation


