module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval, envEmpty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [
      --
      testCase "Pow" $
        runEval (eval (Pow (CstInt 2) (CstInt 3)))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        runEval (eval (Pow (CstInt 2) (CstInt 0)))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        runEval (eval (Pow (CstInt 2) (CstInt (-1))))
          @?= Left "Negative exponent"
      --
    ]