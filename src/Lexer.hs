module Lexer
  ( identifier, number, boolean, string

  , rElse, rFn, rIf, rLet, rReturn

  , asterisk, bang, bangEqual, colon, doubleEqual, equal, greaterThan
  , hyphen, lessThan, plus, semicolon, slash

  , commaSep
  , braces, brackets, parens
  , whiteSpace
  ) where


import qualified Data.Char as Char
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as T

import Control.Monad (mzero, void)
import Text.Parsec ((<|>))
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser, makeTokenParser)


identifier :: Parser String
identifier = T.identifier monkey


number :: Parser Integer
number = T.lexeme monkey (read <$> P.many1 P.digit)


boolean :: Parser Bool
boolean = true <|> false
  where
    true = True <$ reserved "true"
    false = False <$ reserved "false"


string :: Parser String
string = T.lexeme monkey (P.between quote quote (P.many stringChar))
  where
    quote = P.char '"'
    stringChar = P.satisfy (\c -> c /= '"' && Char.isAscii c)


-- RESERVED NAMES


rElse :: Parser ()
rElse = reserved "else"


rFn :: Parser ()
rFn = reserved "fn"


rIf :: Parser ()
rIf = reserved "if"


rLet :: Parser ()
rLet = reserved "let"


rReturn :: Parser ()
rReturn = reserved "return"


-- SYMBOLS


asterisk :: Parser ()
asterisk = symbol "*"


bang :: Parser ()
bang = symbol "!"


bangEqual :: Parser ()
bangEqual = symbol "!="


colon :: Parser ()
colon = symbol ":"


doubleEqual :: Parser ()
doubleEqual = symbol "=="


equal :: Parser ()
equal = symbol "="


greaterThan :: Parser ()
greaterThan = symbol ">"


hyphen :: Parser ()
hyphen = symbol "-"


lessThan :: Parser ()
lessThan = symbol "<"


plus :: Parser ()
plus = symbol "+"


semicolon :: Parser ()
semicolon = symbol ";"


slash :: Parser ()
slash = symbol "/"


-- MISC


commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep monkey


braces :: Parser a -> Parser a
braces = T.braces monkey


brackets :: Parser a -> Parser a
brackets = T.brackets monkey


parens :: Parser a -> Parser a
parens = T.parens monkey


whiteSpace :: Parser ()
whiteSpace = T.whiteSpace monkey


reserved :: String -> Parser ()
reserved = T.reserved monkey


symbol :: String -> Parser ()
symbol = void . T.symbol monkey


-- TOKEN PARSER


monkey :: TokenParser ()
monkey = makeTokenParser monkeyDef


monkeyDef :: LanguageDef ()
monkeyDef =
  emptyDef
    { T.identStart = letter
    , T.identLetter = letter
    , T.reservedNames =
        [ "else"
        , "false"
        , "fn"
        , "if"
        , "let"
        , "return"
        , "true"
        ]
    , T.opStart = mzero
    , T.opLetter = mzero
    }


letter :: Parser Char
letter = P.satisfy isLetter
  where
    isLetter c = Char.isAsciiLower c || Char.isAsciiUpper c || c == '_'
