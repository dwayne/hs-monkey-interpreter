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
  integerSpec
  stringSpec


identifierSpec :: Spec
identifierSpec =
  describe "identifier" $ do
    it "example 1" $ do
      parse Lexer.identifier "x" `shouldBe` Right "x"

    it "example 2" $ do
      parse Lexer.identifier "letter" `shouldBe` Right "letter"

    it "example 3" $ do
      parse Lexer.identifier "true" `shouldSatisfy` isLeft


integerSpec :: Spec
integerSpec =
  describe "integer" $ do
    it "example 1" $ do
      parse Lexer.integer "0" `shouldBe` Right 0

    it "example 2" $ do
      parse Lexer.integer "00" `shouldBe` Right 0

    it "example 3" $ do
      parse Lexer.integer "123" `shouldBe` Right 123


stringSpec :: Spec
stringSpec =
  describe "string" $ do
    it "example 1" $ do
      parse Lexer.string "\"\"" `shouldBe` Right ""

    it "example 2" $ do
      parse Lexer.string "\"foobar\"" `shouldBe` Right "foobar"

    it "example 3" $ do
      parse Lexer.string "\"foo bar\"" `shouldBe` Right "foo bar"


parse :: Parser a -> String -> Either ParseError a
parse p = P.parse p ""
