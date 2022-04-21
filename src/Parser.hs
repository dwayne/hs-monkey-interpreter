module Parser
  ( Program(..)
  , Stmt(..)
  , Expr(..), BinOp(..), UnaryOp(..)
  , Id, Block

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
  | Infix BinOp Expr Expr
  | Prefix UnaryOp Expr
  | Call Expr [Expr]
  | Index Expr Expr
  | If Expr Block (Maybe Block)
  | Function [Id] Block
  deriving (Eq, Show)

data BinOp
  = Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | Add
  | Sub
  | Mul
  | Div
  deriving (Eq, Show)

data UnaryOp
  = Not
  | Negate
  deriving (Eq, Show)

type Id = String
type Block = [Stmt]


parse :: String -> Either ParseError Program
parse = P.parse program ""


program :: Parser Program
program = Program <$ whiteSpace <*> stmts <* P.eof


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
returnStmt = Return <$ rReturn <*> expr <* semicolon


exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> expr <* P.optional semicolon


expr :: Parser Expr
expr = equality


equality :: Parser Expr
equality = comparison `P.chainl1` (eq <|> notEq)
  where
    eq = Infix Equal <$ doubleEqual
    notEq = Infix NotEqual <$ bangEqual


comparison :: Parser Expr
comparison = term `P.chainl1` (lt <|> gt)
  where
    lt = Infix LessThan <$ lessThan
    gt = Infix GreaterThan <$ greaterThan


term :: Parser Expr
term = factor `P.chainl1` (add <|> sub)
  where
    add = Infix Add <$ plus
    sub = Infix Sub <$ hyphen


factor :: Parser Expr
factor = unary `P.chainl1` (mul <|> divide)
  where
    mul = Infix Mul <$ asterisk
    divide = Infix Div <$ slash


unary :: Parser Expr
unary = (notOp <|> negateOp) <*> unary <|> operator
  where
    notOp = Prefix Not <$ bang
    negateOp = Prefix Negate <$ hyphen


-- Operator ::= Operator ( Call | Index ) | Primary
--
-- We have to eliminate the left-recursion in order to write
-- a parser for it.
--
-- When we eliminate it we get,
--
-- Operator ::= Primary ( Call | Index )*
--
-- The extra work needed to parse it is so that we recover the
-- left-associative property of the original production.
operator :: Parser Expr
operator = foldl (flip ($)) <$> primary <*> P.many (call <|> index)
  where
    call :: Parser (Expr -> Expr)
    call = flip Call <$> parens exprList

    index :: Parser (Expr -> Expr)
    index = flip Index <$> brackets expr


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
    params = parens paramList
    paramList = commaSep identifier
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
array = Array <$> brackets exprList


exprList :: Parser [Expr]
exprList = commaSep expr


hash :: Parser Expr
hash = Hash <$> braces keyValueList
  where
    keyValueList = commaSep keyValue
    keyValue = (,) <$> expr <* colon <*> expr


group :: Parser Expr
group = parens expr
