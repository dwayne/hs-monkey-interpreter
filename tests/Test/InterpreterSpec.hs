module Test.InterpreterSpec (spec) where


import Data.Bifunctor (second)
import Interpreter (run, Value(..), Error(..))
import Test.Hspec


spec :: Spec
spec =
  prefixExpressionsSpec


prefixExpressionsSpec :: Spec
prefixExpressionsSpec = do
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
        [ ("-true", UnknownOperator "-BOOLEAN")
        ]

  describe "infix expressions" $ do
    describe "integer arithmetic" $
      makeGoodExamples
        [ -- Examples from page 119
          ("5 + 5", VNum 10)
        , ("5 - 5", VNum 0)
        , ("5 * 5", VNum 25)
        , ("5 / 5", VNum 1)

        -- More examples from page 120
        , ("5 + 5 + 5 + 5 - 10", VNum 10)
        , ("2 * 2 * 2 * 2 * 2", VNum 32)
        , ("-50 + 100 + -50", VNum 0)
        , ("5 * 2 + 10", VNum 20)
        , ("5 + 2 * 10", VNum 25)
        , ("20 + 2 * -10", VNum 0)
        , ("50 / 2 * 2 + 10", VNum 60)
        , ("2 * (5 + 10)", VNum 30)
        , ("3 * 3 * 3 + 10", VNum 37)
        , ("3 * (3 * 3) + 10", VNum 37)
        , ("(5 + 10 * 2 + 15 / 3) * 2 + -10", VNum 50)
        ]

    describe "integer comparison" $ do
      makeGoodExamples
        [ -- Examples from page 121
          ("true", VBool True)
        , ("false", VBool False)
        , ("1 < 2", VBool True)
        , ("1 > 2", VBool False)
        , ("1 < 1", VBool False)
        , ("1 > 1", VBool False)
        , ("1 == 1", VBool True)
        , ("1 != 1", VBool False)
        , ("1 == 2", VBool False)
        , ("1 != 2", VBool True)
        ]

    describe "boolean equality" $ do
      makeGoodExamples
        [ -- Examples from page 122-123
          ("true == true", VBool True)
        , ("false == false", VBool True)
        , ("true == false", VBool False)
        , ("true != false", VBool True)
        , ("false != true", VBool True)
        , ("(1 < 2) == true", VBool True)
        , ("(1 < 2) == false", VBool False)
        , ("(1 > 2) == true", VBool False)
        , ("(1 > 2) == false", VBool True)
        ]

    describe "random examples" $ do -- page 124
      makeGoodExamples
        [ ("5 * 5 + 10", VNum 35)
        , ("3 + 4 * 5 == 3 * 1 + 4 * 5", VBool True)
        , ("5 * 10 > 40 + 5", VBool True)
        , ("(10 + 2) * 30 == 300 + 20 * 3", VBool True)
        , ("(5 > 5 == true) != false", VBool False)
        , ("500 / 2 != 250", VBool False)
        ]

  describe "conditionals" $ do
    makeGoodExamples
      [ ("if (true) { 10 }", VNum 10)
      , ("if (false) { 10 }", VNull)
      , ("if (1) { 10 }", VNum 10)
      , ("if (1 < 2) { 10 }", VNum 10)
      , ("if (1 > 2) { 10 }", VNull)
      , ("if (1 > 2) { 10 } else { 20 }", VNum 20)
      , ("if (1 < 2) { 10 } else { 20 }", VNum 10)
      ]

  describe "return statements" $ do
    makeGoodExamples
      [ ("return 10;", VNum 10)
      , ("return 10; 9", VNum 10)
      , ("return 2 * 5; 9", VNum 10)
      , ("9; return 2 * 5; 9", VNum 10)

      , ( "if (10 > 1) {   \
          \  if (10 > 1) { \
          \    return 10;  \
          \  }             \
          \                \
          \  return 1;     \
          \}               "
        , VNum 10
        )

      -- extra tests to ensure I implemented it correctly
      , ("return if (true) { return 10; };", VNum 10)
      , ("return if (true) { return if (true) { return 10; }; };", VNum 10)
      , ("return if (true) { return if (false) { return 10; }; };", VNull)
      ]

  describe "error handling" $ do
    makeBadExamples
      [ ("5 + true;", TypeMismatch "INTEGER + BOOLEAN")
      , ("5 + true; 5;", TypeMismatch "INTEGER + BOOLEAN")
      , ("-true", UnknownOperator "-BOOLEAN")
      , ("true + false;", UnknownOperator "BOOLEAN + BOOLEAN")
      , ("5; true + false; 5", UnknownOperator "BOOLEAN + BOOLEAN")
      , ("if (10 > 1) { true + false; }", UnknownOperator "BOOLEAN + BOOLEAN")
      , ( "if (10 > 1) {   \
          \  if (10 > 1) { \
          \    return true + false;  \
          \  }             \
          \                \
          \  return 1;     \
          \}               "
        , UnknownOperator "BOOLEAN + BOOLEAN"
        )

      , ("true + 5;", TypeMismatch "BOOLEAN + INTEGER")

      -- extra tests to ensure I implemented it correctly
      , ("5 - true;", TypeMismatch "INTEGER - BOOLEAN")
      , ("true - 5;", TypeMismatch "BOOLEAN - INTEGER")
      , ("true - false;", UnknownOperator "BOOLEAN - BOOLEAN")
      , ("5 * true;", TypeMismatch "INTEGER * BOOLEAN")
      , ("true * 5;", TypeMismatch "BOOLEAN * INTEGER")
      , ("true * false;", UnknownOperator "BOOLEAN * BOOLEAN")
      , ("5 / true;", TypeMismatch "INTEGER / BOOLEAN")
      , ("true / 5;", TypeMismatch "BOOLEAN / INTEGER")
      , ("true / false;", UnknownOperator "BOOLEAN / BOOLEAN")

      , ("5 < true;", TypeMismatch "INTEGER < BOOLEAN")
      , ("true < 5;", TypeMismatch "BOOLEAN < INTEGER")
      , ("true < false;", UnknownOperator "BOOLEAN < BOOLEAN")
      , ("5 > true;", TypeMismatch "INTEGER > BOOLEAN")
      , ("true > 5;", TypeMismatch "BOOLEAN > INTEGER")
      , ("true > false;", UnknownOperator "BOOLEAN > BOOLEAN")
      ]

  describe "mismatched types equality" $ do
    makeGoodExamples
      [ ("true == 5", VBool False)
      , ("5 == true", VBool False)
      , ("true != 5", VBool True)
      , ("5 != true", VBool True)
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
