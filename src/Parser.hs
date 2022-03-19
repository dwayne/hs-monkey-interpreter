module Parser
  ( Program(..), Stmt(..), Expr(..), Id, Block
  , parse
  )
  where


import qualified Lexer
import qualified Text.Parsec as P

import Text.Parsec ((<|>), ParseError)
import Text.Parsec.String (Parser)


data Program
  = Program [Stmt]
  deriving (Eq, Show)

data Stmt
  = Let Id Expr
  | Return Expr
  | ExprStmt Expr
  deriving (Eq, Show)

data Expr
  = Var Id
  | Num Integer
  | Bool Bool
  | Equal Expr Expr
  | NotEqual Expr Expr
  | LessThan Expr Expr
  | GreaterThan Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Not Expr
  | Negate Expr
  | Call Expr [Expr]
  | If Expr Block (Maybe Block)
  | Function [Id] Block
  deriving (Eq, Show)

type Id = String
type Block = [Stmt]


parse :: String -> Either ParseError Program
parse = P.parse program ""


program :: Parser Program
program = Program <$> (Lexer.whiteSpace *> stmts <* P.eof)
  where
    stmts = P.many stmt


stmt :: Parser Stmt
stmt =
  letStmt
  <|> returnStmt
  <|> exprStmt


letStmt :: Parser Stmt
letStmt = Let <$ letToken <*> Lexer.identifier <*> (equal *> expr <* Lexer.semicolon)
  where
    letToken = Lexer.reserved "let"
    equal = Lexer.symbol "="


returnStmt :: Parser Stmt
returnStmt = Return <$> (returnToken *> expr <* Lexer.semicolon)
  where
    returnToken = Lexer.reserved "return"


exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> expr <* optionalSemicolon
  where
    optionalSemicolon = P.optional Lexer.semicolon


expr :: Parser Expr
expr = equality


equality :: Parser Expr
equality = comparison `P.chainl1` equalityOp
  where
    equalityOp = equal <|> notEqual
    equal = Equal <$ Lexer.symbol "=="
    notEqual = NotEqual <$ Lexer.symbol "!="


comparison :: Parser Expr
comparison = term `P.chainl1` comparisonOp
  where
    comparisonOp = lessThan <|> greaterThan
    lessThan = LessThan <$ Lexer.symbol "<"
    greaterThan = GreaterThan <$ Lexer.symbol ">"


term :: Parser Expr
term = factor `P.chainl1` termOp
  where
    termOp = add <|> sub
    add = Add <$ Lexer.symbol "+"
    sub = Sub <$ Lexer.symbol "-"


factor :: Parser Expr
factor = unary `P.chainl1` factorOp
  where
    factorOp = mul <|> divOp
    mul = Mul <$ Lexer.symbol "*"
    divOp = Div <$ Lexer.symbol "/"


unary :: Parser Expr
unary = unaryOp <*> unary <|> call
  where
    unaryOp = notOp <|> negateOp
    notOp = Not <$ Lexer.symbol "!"
    negateOp = Negate <$ Lexer.symbol "-"


call :: Parser Expr
call = foldl Call <$> primary <*> P.many args
  where
    args = Lexer.parens $ Lexer.commaSep expr


primary :: Parser Expr
primary =
  ifExpr
  <|> function
  <|> bool
  <|> variable
  <|> constant
  <|> group


variable :: Parser Expr
variable = Var <$> Lexer.identifier


constant :: Parser Expr
constant = Num <$> Lexer.integer


bool :: Parser Expr
bool = Bool <$> trueOrFalse
  where
    trueOrFalse = true <|> false
    true = const True <$> Lexer.reserved "true"
    false = const False <$> Lexer.reserved "false"


ifExpr :: Parser Expr
ifExpr = If <$ ifToken <*> condition <*> thenBlock <*> maybeElseBlock
  where
    ifToken = Lexer.reserved "if"
    condition = Lexer.parens expr
    thenBlock = block
    maybeElseBlock = P.optionMaybe (elseToken *> block)
    elseToken = Lexer.reserved "else"


function :: Parser Expr
function = Function <$ fn <*> params <*> body
  where
    fn = Lexer.reserved "fn"
    params = Lexer.parens $ Lexer.commaSep Lexer.identifier
    body = block


group :: Parser Expr
group = Lexer.parens expr


block :: Parser Block
block = Lexer.braces $ P.many stmt
