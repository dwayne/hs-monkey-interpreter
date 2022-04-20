module Test.LexerSpec (spec) where


import qualified Lexer
import qualified Text.Parsec as P

import Data.Either (isLeft)
import Test.Hspec
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)


spec :: Spec
spec = do
  identifierSpec
  numberSpec
  booleanSpec
  stringSpec


identifierSpec :: Spec
identifierSpec =
  describe "identifier" $ do
    it "example 1" $ do
      parse Lexer.identifier "x" `shouldBe` Right "x"

    it "example 2" $ do
      parse Lexer.identifier "letter" `shouldBe` Right "letter"

    it "example 3" $ do
      parse Lexer.identifier "let" `shouldSatisfy` isLeft

    it "example 4" $ do
      parse Lexer.identifier "true" `shouldSatisfy` isLeft


numberSpec :: Spec
numberSpec =
  describe "number" $ do
    it "example 1" $ do
      parse Lexer.number "0" `shouldBe` Right 0

    it "example 2" $ do
      parse Lexer.number "00" `shouldBe` Right 0

    it "example 3" $ do
      parse Lexer.number "123" `shouldBe` Right 123


booleanSpec :: Spec
booleanSpec =
  describe "boolean" $ do
    it "example 1" $ do
      parse Lexer.boolean "true" `shouldBe` Right True

    it "example 2" $ do
      parse Lexer.boolean "false" `shouldBe` Right False

    it "example 3" $ do
      parse Lexer.boolean "truer" `shouldSatisfy` isLeft

    it "example 4" $ do
      parse Lexer.boolean "fals" `shouldSatisfy` isLeft


stringSpec :: Spec
stringSpec =
  describe "string" $ do
    it "example 1" $ do
      parse Lexer.string "\"\"" `shouldBe` Right ""

    it "example 2" $ do
      parse Lexer.string "\"   \"" `shouldBe` Right "   "

    it "example 3" $ do
      parse Lexer.string "\"foobar\"" `shouldBe` Right "foobar"

    it "example 4" $ do
      parse Lexer.string "\"foo bar\"" `shouldBe` Right "foo bar"


parse :: Parser a -> String -> Either ParseError a
parse p = P.parse p ""
