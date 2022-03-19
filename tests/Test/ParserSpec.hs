module Test.ParserSpec (spec) where


import Parser
import Test.Hspec


spec :: Spec
spec = do
  describe "examples" $ do
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
                      [ ExprStmt $ Equal (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 15" $ do
      let input = "x != y"
      let program = Program
                      [ ExprStmt $ NotEqual (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 16" $ do
      let input = "x < y"
      let program = Program
                      [ ExprStmt $ LessThan (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 17" $ do
      let input = "x > y"
      let program = Program
                      [ ExprStmt $ GreaterThan (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 18" $ do
      let input = "x + y"
      let program = Program
                      [ ExprStmt $ Add (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 19" $ do
      let input = "x - y"
      let program = Program
                      [ ExprStmt $ Sub (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 20" $ do
      let input = "x * y"
      let program = Program
                      [ ExprStmt $ Mul (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 21" $ do
      let input = "x / y"
      let program = Program
                      [ ExprStmt $ Div (Var "x") (Var "y")
                      ]

      parse input `shouldBe` Right program

    it "example 22" $ do
      let input = "!x"
      let program = Program
                      [ ExprStmt $ Not (Var "x")
                      ]

      parse input `shouldBe` Right program

    it "example 23" $ do
      let input = "-x"
      let program = Program
                      [ ExprStmt $ Negate (Var "x")
                      ]

      parse input `shouldBe` Right program

    it "example 24" $ do
      let input = "1 + 2 * 3 < -4 * 4 / 2"
      let program = Program
                      [ ExprStmt $ LessThan (Add (Num 1) (Mul (Num 2) (Num 3))) (Div (Mul (Negate (Num 4)) (Num 4)) (Num 2))
                      ]

      parse input `shouldBe` Right program

    it "example 25" $ do
      let input = "!(-5 > 3)"
      let program = Program
                      [ ExprStmt $ Not (GreaterThan (Negate (Num 5)) (Num 3))
                      ]

      parse input `shouldBe` Right program

    it "example 26" $ do
      let input = "(1 + 2) * 3"
      let program = Program
                      [ ExprStmt $ Mul (Add (Num 1) (Num 2)) (Num 3)
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
                      , Let "barfoo" (Add (Sub (Add (Div (Mul (Num 5) (Num 5)) (Num 10)) (Num 18)) (Call (Var "add") [Num 5, Num 5])) (Call (Var "multiply") [Num 124]))
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
                      , Let "add" (Function ["a", "b"] [Return (Add (Var "a") (Var "b"))])
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
                      [ ExprStmt $ Add (Mul (Num 5) (Num 5)) (Num 10)
                      , ExprStmt $ Mul (Num 5) (Add (Num 5) (Num 10))
                      , ExprStmt $ Sub (Negate (Num 5)) (Num 10)
                      , ExprStmt $ Mul (Num 5) (Add (Call (Var "add") [Num 2, Num 3]) (Num 10))
                      ]

      parse input `shouldBe` Right program
