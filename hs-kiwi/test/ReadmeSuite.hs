module ReadmeSuite (suite) where

import Kiwi
import Test.Hspec (Spec, specify)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldSatisfy)


suite :: Spec
suite = do
  specify "README / module header example" . withNewSolver $ do
    xl <- variable "xl"
    xm <- editVariable strong "xm"
    xr <- variable "xr"
    constrain required $ xm *. 2 ==@ xl +: xr
    constrain required $ xl +: 10 <=@ xr
    constrain required $ xr <=@ 100
    constrain required $ 0 <=@ xl
    updateVariables
    xlv <- getValue xl
    xmv <- getValue xm
    xrv <- getValue xr

    xmv `shouldBe` (xlv + xrv) / 2
    xlv `shouldSatisfy` (<= xrv - 10)
    xrv `shouldSatisfy` (<= 100)
    xlv `shouldSatisfy` (>= 0)

