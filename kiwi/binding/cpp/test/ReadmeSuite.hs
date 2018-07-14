{-# LANGUAGE OverloadedStrings #-}
module ReadmeSuite (suite) where

import Control.Monad.Trans.Class (lift)
import Kiwi.Cpp
import Test.Hspec (Expectation, Spec, specify)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldReturn, shouldSatisfy)


suite :: Spec
suite = do
  specify "README / module header example" . withNewSolver $ do
    xl <- variable "xl"
    xm <- editVariable strong "xm"
    xr <- variable "xr"
    constrain required (varT xm *. 2 ==@ varT xl +: varT xr) `shouldReturn` Right ()
    constrain required (varT xl +: constE 10 <=@ varT xr) `shouldReturn` Right ()
    constrain required (varT xr <=@ constE 100) `shouldReturn` Right ()
    constrain required (constE 0 <=@ varT xl) `shouldReturn` Right ()
    updateVariables
    xlv <- getValue xl
    xmv <- getValue xm
    xrv <- getValue xr

    xmv `shouldBe` (xlv + xrv) / 2
    xlv `shouldSatisfy` (<= xrv - 10)
    xrv `shouldSatisfy` (<= 100)
    xlv `shouldSatisfy` (>= 0)

    lift (pure () :: Expectation)
