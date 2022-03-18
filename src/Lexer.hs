module Lexer
  ( identifier
  , reserved
  , symbol
  , whiteSpace
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


reserved :: String -> Parser ()
reserved = T.reserved monkey


symbol :: String -> Parser String
symbol = T.symbol monkey


whiteSpace :: Parser ()
whiteSpace = T.whiteSpace monkey


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
