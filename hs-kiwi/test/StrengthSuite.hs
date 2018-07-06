{-# LANGUAGE OverloadedStrings #-}
module StrengthSuite (suite) where

import Kiwi
import Kiwi.Raw.Constraint (RelationalOperator(..))
import Test.Hspec (Spec, specify)
import Test.Hspec.Expectations.Lifted (shouldSatisfy)


suite :: Spec
suite = do
  specify "accessing predefined strength values" $ do
    weak `shouldSatisfy` (< medium)
    medium `shouldSatisfy` (< strong)
    strong `shouldSatisfy` (< required)

  specify "creating strength" $ do
    mkStrength 0 0 1 `shouldSatisfy` (< mkStrength 0 1 0)
    mkStrength 0 1 0 `shouldSatisfy` (< mkStrength 1 0 0)
    mkStrengthWithWeight 1 0 0 1 `shouldSatisfy` (< mkStrengthWithWeight 1 0 0 4)
