module Parser
  ( Program(..), Stmt(..), Expr(..), Id, Block
  , parse
  , ParseError
  )
  where


import qualified Text.Parsec as P

import Lexer
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
  | String String
  | Array [Expr]
  | Hash [(Expr, Expr)]
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
  | Index Expr Expr
  | If Expr Block (Maybe Block)
  | Function [Id] Block
  deriving (Eq, Show)

type Id = String
type Block = [Stmt]


parse :: String -> Either ParseError Program
parse = P.parse program ""


program :: Parser Program
program = Program <$> (whiteSpace *> stmts <* P.eof)


stmts :: Parser [Stmt]
stmts = P.many stmt


stmt :: Parser Stmt
stmt =
  letStmt
  <|> returnStmt
  <|> exprStmt


letStmt :: Parser Stmt
letStmt = Let <$ rLet <*> identifier <*> (equal *> expr <* semicolon)


returnStmt :: Parser Stmt
returnStmt = Return <$> (rReturn *> expr <* semicolon)


exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> expr <* P.optional semicolon


expr :: Parser Expr
expr = equality


equality :: Parser Expr
equality = comparison `P.chainl1` (eq <|> notEq)
  where
    eq = Equal <$ doubleEqual
    notEq = NotEqual <$ bangEqual


comparison :: Parser Expr
comparison = term `P.chainl1` (lt <|> gt)
  where
    lt = LessThan <$ lessThan
    gt = GreaterThan <$ greaterThan


term :: Parser Expr
term = factor `P.chainl1` (add <|> sub)
  where
    add = Add <$ plus
    sub = Sub <$ hyphen


factor :: Parser Expr
factor = unary `P.chainl1` (mul <|> divide)
  where
    mul = Mul <$ asterisk
    divide = Div <$ slash


unary :: Parser Expr
unary = (notOp <|> negateOp) <*> unary <|> operator
  where
    notOp = Not <$ bang
    negateOp = Negate <$ hyphen


operator :: Parser Expr
operator = foldl (flip ($)) <$> primary <*> (P.many (args <|> index))
  where
    args :: Parser (Expr -> Expr)
    args = flip Call <$> (parens $ commaSep expr)

    index :: Parser (Expr -> Expr)
    index = flip Index <$> (brackets expr)


primary :: Parser Expr
primary =
  ifExpr
  <|> function
  <|> bool
  <|> var
  <|> num
  <|> str
  <|> array
  <|> hash
  <|> group


ifExpr :: Parser Expr
ifExpr = If <$ rIf <*> condition <*> thenBlock <*> maybeElseBlock
  where
    condition = parens expr
    thenBlock = block
    maybeElseBlock = P.optionMaybe (rElse *> block)


function :: Parser Expr
function = Function <$ rFn <*> params <*> body
  where
    params = parens $ commaSep identifier
    body = block


block :: Parser Block
block = braces stmts


bool :: Parser Expr
bool = Bool <$> boolean


var :: Parser Expr
var = Var <$> identifier


num :: Parser Expr
num = Num <$> number


str :: Parser Expr
str = String <$> string


array :: Parser Expr
array = Array <$> exprList
  where
    exprList = brackets $ commaSep expr


hash :: Parser Expr
hash = Hash <$> keyValueList
  where
    keyValueList = braces $ commaSep keyValue
    keyValue = (,) <$> expr <* colon <*> expr


group :: Parser Expr
group = parens expr
