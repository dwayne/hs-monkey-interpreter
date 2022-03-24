module Test.InterpreterSpec (spec) where


import Data.Bifunctor (second)
import Interpreter (run, Value(..), Error(..))
import Test.Hspec


spec :: Spec
spec =
  prefixExpressionsSpec


prefixExpressionsSpec :: Spec
prefixExpressionsSpec =
  describe "prefix expressions" $ do
    describe "! (not)" $
      makeGoodExamples
        [ ("!true", VBool False)
        , ("!false", VBool True)
        , ("!5", VBool False)
        , ("!!true", VBool True)
        , ("!!false", VBool False)
        , ("!!5", VBool True)

        -- More examples from page 119
        , ("!-5", VBool False)
        , ("!!-5", VBool True)
        , ("!!!!-5", VBool True)
        ]

    describe "- (negate)" $ do
      makeGoodExamples
        [ ("5", VNum 5)
        , ("10", VNum 10)
        , ("-5", VNum (-5))
        , ("-10", VNum (-10))
        ]

      makeBadExamples
        [ ("-true", ExpectedNum $ VBool True)
        ]


makeGoodExamples :: [(String, Value)] -> SpecWith (Arg Expectation)
makeGoodExamples = makeExamples . map (second Right)


makeBadExamples :: [(String, Error)] -> SpecWith (Arg Expectation)
makeBadExamples = makeExamples . map (second Left)


makeExamples :: [(String, Either Error Value)] -> SpecWith (Arg Expectation)
makeExamples examples =
  let
    numberedExamples = zip [1..] examples

    makeExample (n, (input, expectedOutput)) =
      it ("example " ++ show n) $
        run input `shouldBe` expectedOutput

    specs = map makeExample numberedExamples
  in
  foldl1 (>>) specs
