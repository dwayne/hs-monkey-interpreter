module Interpreter where


import Parser
import Text.Parsec (ParseError)


data Value
  = VNum Integer
  | VBool Bool
  | VNull
  deriving (Show)


data Error
  = SyntaxError ParseError
  deriving (Show)


run :: String -> Either Error Value
run input =
  case parse input of
    Right program ->
      runProgram program

    Left parseError ->
      Left $ SyntaxError parseError


runProgram :: Program -> Either Error Value
runProgram (Program stmts) = runStmts stmts


runStmts :: [Stmt] -> Either Error Value
runStmts [] = Right VNull
runStmts [stmt] = runStmt stmt
runStmts (stmt : rest) = runStmt stmt >> runStmts rest


runStmt :: Stmt -> Either Error Value
runStmt stmt =
  case stmt of
    Let _ _ ->
      Right VNull

    Return _ ->
      Right VNull

    ExprStmt expr ->
      runExpr expr


runExpr :: Expr -> Either Error Value
runExpr expr =
  case expr of
    Num n ->
      Right $ VNum n

    Bool b ->
      Right $ VBool b

    _ ->
      Right VNull
