module Lexer
  ( identifier
  , integer
  , string
  , reserved
  , symbol
  , whiteSpace
  , parens
  , braces
  , semicolon
  , commaSep
  )
  where


import qualified Data.Char as Char
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as T

import Control.Monad (mzero)
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser, makeTokenParser)


identifier :: Parser String
identifier = T.identifier monkey


integer :: Parser Integer
integer = T.lexeme monkey (read <$> P.many1 P.digit)


string :: Parser String
string = T.lexeme monkey (P.between quote quote (P.many stringChar))
  where
    quote = P.char '"'
    stringChar = P.satisfy (\c -> c /= '"' && Char.isAscii c)


reserved :: String -> Parser ()
reserved = T.reserved monkey


symbol :: String -> Parser String
symbol = T.symbol monkey


whiteSpace :: Parser ()
whiteSpace = T.whiteSpace monkey


parens :: Parser a -> Parser a
parens = T.parens monkey


braces :: Parser a -> Parser a
braces = T.braces monkey


semicolon :: Parser String
semicolon = T.semi monkey


commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep monkey


monkey :: TokenParser ()
monkey = makeTokenParser monkeyDef


monkeyDef :: LanguageDef ()
monkeyDef =
  emptyDef
    { T.identStart = letter
    , T.identLetter = letter
    , T.opStart = mzero
    , T.opLetter = mzero
    , T.reservedNames =
        [ "else"
        , "false"
        , "fn"
        , "if"
        , "let"
        , "return"
        , "true"
        ]
    }


letter :: Parser Char
letter = P.satisfy isLetter
  where
    isLetter c = Char.isAsciiLower c || Char.isAsciiUpper c || c == '_'
