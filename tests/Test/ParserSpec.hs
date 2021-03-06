module Test.ParserSpec (spec) where


import Parser
import Test.Hspec


spec :: Spec
spec = do
  describe "parser" $ do
    it "example 1" $ do
      let input = "x"
      let program = Program
                      [ ExprStmt $ Var "x"
                      ]

      parse input `shouldBe` Right program

    it "example 2" $ do
      let input = "123"
      let program = Program
                      [ ExprStmt $ Num 123
                      ]

      parse input `shouldBe` Right program

    it "example 3" $ do
      let input = "true"
      let program = Program
                      [ ExprStmt $ Bool True
                      ]

      parse input `shouldBe` Right program

    it "example 4" $ do
      let input = "false"
      let program = Program
                      [ ExprStmt $ Bool False
                      ]

      parse input `shouldBe` Right program

    it "example 5" $ do
      let input = "if (x) {}"
      let program = Program
                      [ ExprStmt $ If (Var "x") [] Nothing
                      ]

      parse input `shouldBe` Right program

    it "example 6" $ do
      let input = "if (x) {} else {}"
      let program = Program
                      [ ExprStmt $ If (Var "x") [] (Just [])
                      ]

      parse input `shouldBe` Right program

    it "example 7" $ do
      let input = "if (x) { 1 } else { 2; }"
      let program = Program
                      [ ExprStmt $ If (Var "x") [ ExprStmt (Num 1) ] (Just [ ExprStmt (Num 2) ])
                      ]

      parse input `shouldBe` Right program

    it "example 8" $ do
      let input = "fn () {}"
      let program = Program
                      [ ExprStmt $ Function [] []
                      ]

      parse input `shouldBe` Right program

    it "example 9" $ do
      let input = "fn (x) { x }"
      let program = Program
                      [ ExprStmt $ Function ["x"] [ ExprStmt (Var "x") ]
                      ]

      parse input `shouldBe` Right program

    it "example 10" $ do
      let input = "fn (x, y) { x; y }"
      let program = Program
                      [ ExprStmt $ Function ["x", "y"] [ ExprStmt (Var "x"), ExprStmt (Var "y") ]
                      ]

      parse input `shouldBe` Right program

    it "example 11" $ do
      let input = "   \
        \ let x = 10; \
        \ let y = 15; "
      let program = Program
                      [ Let "x" (Num 10)
                      , Let "y" (Num 15)
                      ]

      parse input `shouldBe` Right program

    it "example 12" $ do
      let input = "return 5;"
      let program = Program
                      [ Return (Num 5)
                      ]

      parse input `shouldBe` Right program

    it "example 13" $ do
      let input = "fn (x) { return x; }"
      let program = Program
                      [ ExprStmt $ Function ["x"] [ Return (Var "x") ]
                      ]

      parse input `shouldBe` Right program

    it "example 14" $ do
      let input = "x == y"
      let program = Program
                      [ ExprStmt $ Infix Equal (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 15" $ do
      let input = "x != y"
      let program = Program
                      [ ExprStmt $ Infix NotEqual (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 16" $ do
      let input = "x < y"
      let program = Program
                      [ ExprStmt $ Infix LessThan (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 17" $ do
      let input = "x > y"
      let program = Program
                      [ ExprStmt $ Infix GreaterThan (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 18" $ do
      let input = "x + y"
      let program = Program
                      [ ExprStmt $ Infix Add (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 19" $ do
      let input = "x - y"
      let program = Program
                      [ ExprStmt $ Infix Sub (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 20" $ do
      let input = "x * y"
      let program = Program
                      [ ExprStmt $ Infix Mul (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 21" $ do
      let input = "x / y"
      let program = Program
                      [ ExprStmt $ Infix Div (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 22" $ do
      let input = "!x"
      let program = Program
                      [ ExprStmt $ Prefix Not (Var "x")
                      ]

      parse input `shouldBe` Right program

    it "example 23" $ do
      let input = "-x"
      let program = Program
                      [ ExprStmt $ Prefix Negate (Var "x")
                      ]

      parse input `shouldBe` Right program

    it "example 24" $ do
      let input = "1 + 2 * 3 < -4 * 4 / 2"
      let program = Program
                      [ ExprStmt $ Infix LessThan (Infix Add (Num 1) (Infix Mul (Num 2) (Num 3))) (Infix Div (Infix Mul (Prefix Negate (Num 4)) (Num 4)) (Num 2))
                      ]

      parse input `shouldBe` Right program

    it "example 25" $ do
      let input = "!(-5 > 3)"
      let program = Program
                      [ ExprStmt $ Prefix Not (Infix GreaterThan (Prefix Negate (Num 5)) (Num 3))
                      ]

      parse input `shouldBe` Right program

    it "example 26" $ do
      let input = "(1 + 2) * 3"
      let program = Program
                      [ ExprStmt $ Infix Mul (Infix Add (Num 1) (Num 2)) (Num 3)
                      ]

      parse input `shouldBe` Right program

    it "example 27" $ do
      let input = "f()"
      let program = Program
                      [ ExprStmt $ Call (Var "f") []
                      ]

      parse input `shouldBe` Right program

    it "example 28" $ do
      let input = "f(1)"
      let program = Program
                      [ ExprStmt $ Call (Var "f") [Num 1]
                      ]

      parse input `shouldBe` Right program

    it "example 29" $ do
      let input = "f(1, 2)"
      let program = Program
                      [ ExprStmt $ Call (Var "f") [Num 1, Num 2]
                      ]

      parse input `shouldBe` Right program

    it "example 30" $ do
      let input = "f(1)(2)(3)"
      let program = Program
                      [ ExprStmt $ Call (Call (Call (Var "f") [Num 1]) [Num 2]) [Num 3]
                      ]

      parse input `shouldBe` Right program

    it "example 31" $ do
      let input = "\"Hello, world!\""
      let program = Program
                      [ ExprStmt $ String "Hello, world!"
                      ]

      parse input `shouldBe` Right program

    it "example 32" $ do
      let input = "[]"
      let program = Program
                      [ ExprStmt $ Array []
                      ]

      parse input `shouldBe` Right program

    it "example 33" $ do
      let input = "[1]"
      let program = Program
                      [ ExprStmt $ Array [Num 1]
                      ]

      parse input `shouldBe` Right program

    it "example 34" $ do
      let input = "[1, 2]"
      let program = Program
                      [ ExprStmt $ Array [Num 1, Num 2]
                      ]

      parse input `shouldBe` Right program

    it "example 35" $ do
      let input = "[\"Thorsten\", \"Ball\", 28, fn(x) { x * x }]"
      let program = Program
                      [ ExprStmt $
                          Array
                            [ String "Thorsten"
                            , String "Ball"
                            , Num 28
                            , Function ["x"] [ ExprStmt $ Infix Mul (Var "x") (Var "x") ]
                            ]
                      ]

      parse input `shouldBe` Right program

    it "example 36" $ do
      let input = "myArray[1 + 1]"
      let program = Program
                      [ ExprStmt $ Index (Var "myArray") (Infix Add (Num 1) (Num 1))
                      ]

      parse input `shouldBe` Right program

    it "example 37" $ do
      let input = "a * [1, 2, 3, 4][b * c] * d"
      let program = Program
                      [ ExprStmt $
                          Infix Mul
                            (Infix Mul (Var "a") (Index (Array [Num 1, Num 2, Num 3, Num 4]) (Infix Mul (Var "b") (Var "c"))))
                            (Var "d")
                      ]

      parse input `shouldBe` Right program

    it "example 38" $ do
      let input = "add(a * b[2], b[1], 2 * [1, 2][1])"
      let program = Program
                      [ ExprStmt $
                          Call
                            (Var "add")
                            [ Infix Mul (Var "a") (Index (Var "b") (Num 2))
                            , Index (Var "b") (Num 1)
                            , Infix Mul (Num 2) (Index (Array [Num 1, Num 2]) (Num 1))
                            ]
                      ]

      parse input `shouldBe` Right program

    it "example 39" $ do
      let input = "a[0][1][2]"
      let program = Program
                      [ ExprStmt $
                          Index
                            (Index (Index (Var "a") (Num 0)) (Num 1))
                            (Num 2)
                      ]

      parse input `shouldBe` Right program

    it "example 40" $ do
      let input = "a(0)[1](2)[3]"
      let program = Program
                      [ ExprStmt $
                          Index
                            (Call (Index (Call (Var "a") [Num 0]) (Num 1)) [Num 2])
                            (Num 3)
                      ]

      parse input `shouldBe` Right program

    it "example 41" $ do
      let input = "a[0](1)[2](3)"
      let program = Program
                      [ ExprStmt $
                          Call
                            (Index (Call (Index (Var "a") (Num 0)) [Num 1]) (Num 2))
                            [Num 3]
                      ]

      parse input `shouldBe` Right program

  describe "hash literals" $ do
    it "example 1" $ do
      let input = "{}"
      let program = Program
                      [ ExprStmt $ Hash []
                      ]

      parse input `shouldBe` Right program

    it "example 2" $ do
      let input = "{\"name\": \"Jimmy\", \"age\": 72, \"band\": \"Led Zeppelin\"}"
      let program = Program
                      [ ExprStmt $ Hash
                          [ (String "name", String "Jimmy")
                          , (String "age", Num 72)
                          , (String "band", String "Led Zeppelin")
                          ]
                      ]

      parse input `shouldBe` Right program

    it "example 3" $ do
      let input = "{true: \"yes, a boolean\", 99: \"correct, an integer\"}"
      let program = Program
                      [ ExprStmt $ Hash
                          [ (Bool True, String "yes, a boolean")
                          , (Num 99, String "correct, an integer")
                          ]
                      ]

      parse input `shouldBe` Right program

    it "example 4" $ do
      let input = "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}"
      let program = Program
                      [ ExprStmt $ Hash
                          [ (String "one", Infix Add (Num 0) (Num 1))
                          , (String "two", Infix Sub (Num 10) (Num 8))
                          , (String "three", Infix Div (Num 15) (Num 5))
                          ]
                      ]

      parse input `shouldBe` Right program

  describe "book examples" $ do
    it "example 1" $ do
      let input = "                                                 \
        \ let x = 5;                                                \
        \ let y = 10;                                               \
        \ let foobar = add(5, 5);                                   \
        \ let barfoo = 5 * 5 / 10 + 18 - add(5, 5) + multiply(124); \
        \ let anotherName = barfoo;                                 "
      let program = Program
                      [ Let "x" (Num 5)
                      , Let "y" (Num 10)
                      , Let "foobar" (Call (Var "add") [Num 5, Num 5])
                      , Let "barfoo" (Infix Add (Infix Sub (Infix Add (Infix Div (Infix Mul (Num 5) (Num 5)) (Num 10)) (Num 18)) (Call (Var "add") [Num 5, Num 5])) (Call (Var "multiply") [Num 124]))
                      , Let "anotherName" (Var "barfoo")
                      ]

      parse input `shouldBe` Right program

    it "example 2" $ do
      let input = "            \
        \ let x = 10;          \
        \ let y = 15;          \
        \ let add = fn(a, b) { \
        \   return a + b;      \
        \ };                   "
      let program = Program
                      [ Let "x" (Num 10)
                      , Let "y" (Num 15)
                      , Let "add" (Function ["a", "b"] [Return (Infix Add (Var "a") (Var "b"))])
                      ]

      parse input `shouldBe` Right program

    it "example 3" $ do
      let input = "       \
        \ return 5;       \
        \ return 10;      \
        \ return add(15); "
      let program = Program
                      [ Return (Num 5)
                      , Return (Num 10)
                      , Return (Call (Var "add") [Num 15])
                      ]

      parse input `shouldBe` Right program

    it "example 4" $ do
      let input = "            \
        \ 5 * 5 + 10           \
        \ 5 * (5 + 10);        \
        \ -5 - 10              \
        \ 5 * (add(2, 3) + 10) "
      -- NOTICE: I had to add a semicolon to make it parse correctly.
      -- Otherwise, it would be interpreted as 5 * (5 + 10) - 5 - 10.
      let program = Program
                      [ ExprStmt $ Infix Add (Infix Mul (Num 5) (Num 5)) (Num 10)
                      , ExprStmt $ Infix Mul (Num 5) (Infix Add (Num 5) (Num 10))
                      , ExprStmt $ Infix Sub (Prefix Negate (Num 5)) (Num 10)
                      , ExprStmt $ Infix Mul (Num 5) (Infix Add (Call (Var "add") [Num 2, Num 3]) (Num 10))
                      ]

      parse input `shouldBe` Right program
