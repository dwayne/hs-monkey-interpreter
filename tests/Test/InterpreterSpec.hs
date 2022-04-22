module Test.InterpreterSpec (spec) where


import qualified Environment as Env
import qualified Hash
import qualified Runtime

import Interpreter (run, Value(..), Error(..))
import Runtime hiding (Error)
import Test.Hspec


spec :: Spec
spec = do
  describe "prefix expressions" $ do
    describe "! (not)" $ do
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
    describe "integer arithmetic" $ do
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

  describe "let statements" $ do
    makeGoodExamples
      [ ("let a = 5; a;", VNum 5)
      , ("let a = 5 * 5; a;", VNum 25)
      , ("let a = 5; let b = a; b;", VNum 5)
      , ("let a = 5; let b = a; let c = a + b + 5; c;", VNum 15)

      -- more examples from page 140
      , ( "let a = 5;                \
          \let b = a > 3;            \
          \let c = a * 99;           \
          \if (b) { 10 } else { 1 }; "
        , VNum 10
        )
      , ( "let a = 5;                              \
          \let b = a > 3;                          \
          \let c = a * 99;                         \
          \let d = if (c > a) { 99 } else { 100 }; \
          \d                                       "
        , VNum 99
        )
      , ( "let a = 5;                              \
          \let b = a > 3;                          \
          \let c = a * 99;                         \
          \let d = if (c > a) { 99 } else { 100 }; \
          \d * c * a;                              "
        , VNum 245025
        )
      ]

    makeBadExamples
      [ ("foobar", IdentifierNotFound "foobar")
      ]

  describe "function definitions" $ do
    it "example 1" $ do
      Right val <- snd <$> (run "fn(x) { x + 2; };" Env.empty)

      -- just a sanity check
      show val `shouldBe` "<function>"

  describe "functions calls" $ do
    makeGoodExamples
      [ ("let identity = fn(x) { x; }; identity(5);", VNum 5)
      , ("let double = fn(x) { x * 2; }; double(5);", VNum 10)
      , ("let add = fn(x, y) { x + y; }; add(5, 5);", VNum 10)
      , ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", VNum 20)
      , ("fn(x) { x; }(5)", VNum 5)

      -- Why extend the function’s environment and not the current environment?
      -- page 148
      , ("let newAdder = fn(x) {     \
         \  fn(y) { x + y };         \
         \ };                        \
         \ let addTwo = newAdder(2); \
         \ addTwo(2);                "
        , VNum 4
        )

      , ( "let add = fn(a, b) { a + b };                  \
          \let sub = fn(a, b) { a - b };                  \
          \let applyFunc = fn(a, b, func) { func(a, b) }; \
          \applyFunc(2, 2, add);                          "
        , VNum 4
        )
      , ( "let add = fn(a, b) { a + b };                  \
          \let sub = fn(a, b) { a - b };                  \
          \let applyFunc = fn(a, b, func) { func(a, b) }; \
          \applyFunc(10, 2, sub);                          "
        , VNum 8
        )
      ]

    makeBadExamples
      [ ( "let f = fn (x) { x }; \
          \f()                   "
        , ArgumentError "wrong number of arguments. got=0, want=1"
        )
      , ( "let f = fn (x) { x }; \
          \f(1, 2)               "
        , ArgumentError "wrong number of arguments. got=2, want=1"
        )
      ]

  describe "recursion" $ do
    makeGoodExamples
      [ ( "let factorial = fn(n) { if (n == 0) { 1 } else { n * factorial(n - 1) } }; \
          \factorial(5)                                                               "
        , VNum 120
        )
      ]

  describe "strings" $ do
    makeGoodExamples
      [ ( "let firstName = \"Thorsten\";                            \
          \let lastName = \"Ball\";                                 \
          \let fullName = fn(first, last) { first + \" \" + last }; \
          \fullName(firstName, lastName);                           "
        , VString "Thorsten Ball"
        )
      , ( "\"Hello world!\""
        , VString "Hello world!"
        )
      , ( "let hello = \"Hello there, fellow Monkey users and fans!\"; \
          \hello                                                       "
        , VString "Hello there, fellow Monkey users and fans!"
        )
      , ( "let giveMeHello = fn() { \"Hello!\" }; \
          \giveMeHello()                          "
        , VString "Hello!"
        )

      -- more examples from the book, page 159
      , ( "let makeGreeter = fn(greeting) { fn(name) { greeting + \" \" + name + \"!\" } }; \
          \let hello = makeGreeter(\"Hello\");                                              \
          \hello(\"Thorsten\");                                                             "
        , VString "Hello Thorsten!"
        )
      , ( "let makeGreeter = fn(greeting) { fn(name) { greeting + \" \" + name + \"!\" } }; \
          \let heythere = makeGreeter(\"Hey there\");                                       \
          \heythere(\"Thorsten\");                                                          "
        , VString "Hey there Thorsten!"
        )
      ]

    makeBadExamples
      [ ("\"Hello\" - \"World\";", UnknownOperator "STRING - STRING")
      ]

  describe "builtin functions" $ do
    describe "len" $ do
      makeGoodExamples
        [ ("len(\"\")", VNum 0)
        , ("len(\"four\")", VNum 4)
        , ("len(\"hello world\")", VNum 11)
        ]

      makeBadExamples
        [ ("len(1)", BuiltinError "argument to `len` not supported, got INTEGER")
        , ("len(\"one\", \"two\")", BuiltinError "wrong number of arguments. got=2, want=1")
        ]

  describe "arrays" $ do
    makeGoodExamples
      [ ("[]", VArray [])
      , ("[1]", VArray [VNum 1])
      , ("[1, 2]", VArray [VNum 1, VNum 2])
      , ("[1, false, true, \"four\", [], [[]]]"
        , VArray
            [ VNum 1
            , VBool False
            , VBool True
            , VString "four"
            , VArray []
            , VArray [VArray []]
            ]
        )
      , ( "let myArray = [\"one\", \"two\", \"three\"]; \
          \len(myArray)                                 "
        , VNum 3
        )
      ]

    describe "builtins" $ do
      describe "first" $ do
        makeGoodExamples
          [ ("first([])", VNull)
          , ("first([1])", VNum 1)
          , ("first([1, 2])", VNum 1)
          ]

        makeBadExamples
          [ ("first(1)", BuiltinError "argument to `first` must be ARRAY, got INTEGER")
          , ("first([], 1)", BuiltinError "wrong number of arguments. got=2, want=1")
          ]

      describe "last" $ do
        makeGoodExamples
          [ ("last([])", VNull)
          , ("last([1])", VNum 1)
          , ("last([1, 2])", VNum 2)
          , ("last([1, 2, 3])", VNum 3)
          ]

        makeBadExamples
          [ ("last(1)", BuiltinError "argument to `last` must be ARRAY, got INTEGER")
          , ("last([], 1)", BuiltinError "wrong number of arguments. got=2, want=1")
          ]

      describe "rest" $ do
        makeGoodExamples
          [ ("rest([])", VNull)
          , ("rest([1])", VArray [])
          , ("rest([1, 2])", VArray [VNum 2])
          , ("rest([1, 2, 3])", VArray [VNum 2, VNum 3])
          , ("rest(rest([1, 2, 3]))", VArray [VNum 3])
          ]

        makeBadExamples
          [ ("rest(1)", BuiltinError "argument to `rest` must be ARRAY, got INTEGER")
          , ("rest([], 1)", BuiltinError "wrong number of arguments. got=2, want=1")
          ]

      describe "push" $ do
        makeGoodExamples
          [ ("push([], 1)", VArray [VNum 1])
          , ("push([1], 2)", VArray [VNum 1, VNum 2])
          , ("push([1, 2], 3)", VArray [VNum 1, VNum 2, VNum 3])
          , ("push(push(push([], 1), 2), 3)", VArray [VNum 1, VNum 2, VNum 3])
          ]

        makeBadExamples
          [ ("push(1, true)", BuiltinError "argument to `push` must be ARRAY, got INTEGER")
          , ("push([])", BuiltinError "wrong number of arguments. got=1, want=2")
          , ("push([], 1, true)", BuiltinError "wrong number of arguments. got=3, want=2")
          ]

  describe "hashes" $ do
    makeGoodExamples
      [ ( "{}"
        , VHash $ Hash.fromList []
        )
      , ( "{\"name\": \"Jimmy\", \"age\": 72, \"band\": \"Led Zeppelin\"}"
        , VHash $ Hash.fromList
            [ (Hash.KString "name", VString "Jimmy")
            , (Hash.KString "age", VNum 72)
            , (Hash.KString "band", VString "Led Zeppelin")
            ]
        )
      , ( "{true: \"yes, a boolean\", 99: \"correct, an integer\"}"
        , VHash $ Hash.fromList
            [ (Hash.KBool True, VString "yes, a boolean")
            , (Hash.KNum 99, VString "correct, an integer")
            ]
        )
      , ( "let two = \"two\";       \
          \{ \"one\": 10 - 9        \
          \, two: 1 + 1             \
          \,\"thr\" + \"ee\": 6 / 2 \
          \,4: 4                    \
          \,true: 5                 \
          \,false: 6                \
          \}                        "
        , VHash $ Hash.fromList
            [ (Hash.KString "one", VNum 1)
            , (Hash.KString "two", VNum 2)
            , (Hash.KString "three", VNum 3)
            , (Hash.KNum 4, VNum 4)
            , (Hash.KBool True, VNum 5)
            , (Hash.KBool False, VNum 6)
            ]
        )
      ]

  describe "index operator" $ do
    describe "for arrays" $ do
      makeGoodExamples
        [ ( "[1, 2, 3][0]"
          , VNum 1
          )
        , ( "[1, 2, 3][1]"
          , VNum 2
          )
        , ( "[1, 2, 3][2]"
          , VNum 3
          )
        , ( "let i = 0; [1][i];"
          , VNum 1
          )
        , ( "[1, 2, 3][1 + 1];"
          , VNum 3
          )
        , ( "let myArray = [1, 2, 3]; myArray[2];"
          , VNum 3
          )
        , ( "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];"
          , VNum 6
          )
        , ( "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]"
          , VNum 2
          )
        , ( "[1, 2, 3][3]"
          , VNull
          )
        , ( "[1, 2, 3][-1]"
          , VNull
          )
        ]

    describe "for hashes" $ do
      makeGoodExamples
        [ ( "{\"foo\": 5}[\"foo\"]"
          , VNum 5
          )
        , ( "{\"foo\": 5}[\"bar\"]"
          , VNull
          )
        , ( "let key = \"foo\"; {\"foo\": 5}[key]"
          , VNum 5
          )
        , ( "{}[\"foo\"]"
          , VNull
          )
        , ( "{5: 5}[5]"
          , VNum 5
          )
        , ( "{true: 5}[true]"
          , VNum 5
          )
        , ( "{false: 5}[false]"
          , VNum 5
          )
        ]

    makeBadExamples
      [ ( "1[0]"
        , TypeMismatch "INTEGER[INTEGER]"
        )
      , ( "[1][false]"
        , TypeMismatch "ARRAY[BOOLEAN]"
        )
      , ( "{\"name\": \"Monkey\"}[fn(x) { x }]"
        , TypeMismatch "HASH[FUNCTION]"
        )
      ]

    describe "misc examples" $ do
      -- see page 197
      makeGoodExamples
        [ ( "let people = [{\"name\": \"Alice\", \"age\": 24}, {\"name\": \"Anna\", \"age\": 28}]; \
            \people[0][\"name\"];                                                                  "
          , VString "Alice"
          )
        , ( "let people = [{\"name\": \"Alice\", \"age\": 24}, {\"name\": \"Anna\", \"age\": 28}]; \
            \people[1][\"age\"];                                                                   "
          , VNum 28
          )
        , ( "let people = [{\"name\": \"Alice\", \"age\": 24}, {\"name\": \"Anna\", \"age\": 28}]; \
            \people[1][\"age\"] + people[0][\"age\"];                                              "
          , VNum 52
          )
        , ( "let people = [{\"name\": \"Alice\", \"age\": 24}, {\"name\": \"Anna\", \"age\": 28}]; \
            \let getName = fn(person) { person[\"name\"]; };                                       \
            \getName(people[0]);                                                                   "
          , VString "Alice"
          )
        , ( "let people = [{\"name\": \"Alice\", \"age\": 24}, {\"name\": \"Anna\", \"age\": 28}]; \
            \let getName = fn(person) { person[\"name\"]; };                                       \
            \getName(people[1]);                                                                   "
          , VString "Anna"
          )
        ]

  describe "test-driving arrays" $ do
    -- see page 180-181
    makeGoodExamples
      [ ( "let map = fn(arr, f) {                                   \
          \  let iter = fn(arr, accumulated) {                      \
          \    if (len(arr) == 0) {                                 \
          \      accumulated                                        \
          \    } else {                                             \
          \      iter(rest(arr), push(accumulated, f(first(arr)))); \
          \    }                                                    \
          \  };                                                     \
          \  iter(arr, []);                                         \
          \};                                                       \
          \let a = [1, 2, 3, 4];                                    \
          \let double = fn(x) { x * 2 };                            \
          \map(a, double);                                          "
        , VArray [VNum 2, VNum 4, VNum 6, VNum 8]
        )
      , ( "let reduce = fn(arr, initial, f) {                  \
          \  let iter = fn(arr, result) {                      \
          \    if (len(arr) == 0) {                            \
          \      result                                        \
          \    } else {                                        \
          \      iter(rest(arr), f(result, first(arr)));       \
          \    }                                               \
          \  };                                                \
          \  iter(arr, initial);                               \
          \};                                                  \
          \let sum = fn(arr) {                                 \
          \  reduce(arr, 0, fn(initial, el) { initial + el }); \
          \};                                                  \
          \sum([1, 2, 3, 4, 5]);                               "
        , VNum 15
        )
      ]

makeGoodExamples :: [(String, Value)] -> SpecWith (Arg Expectation)
makeGoodExamples = makeExamples . fmap (fmap Right)


makeBadExamples :: [(String, Runtime.Error)] -> SpecWith (Arg Expectation)
makeBadExamples = makeExamples . fmap (fmap (Left . RuntimeError))


makeExamples :: [(String, Either Error Value)] -> SpecWith (Arg Expectation)
makeExamples examples =
  let
    numberedExamples = zip [1..] examples

    makeExample (n, (input, expectedOutput)) =
      it ("example " ++ show n) $
        (snd <$> (run input Env.empty)) `shouldReturn` expectedOutput

    specs = map makeExample numberedExamples
  in
  foldl1 (>>) specs
