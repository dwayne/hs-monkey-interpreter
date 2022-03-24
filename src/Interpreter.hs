module Interpreter
  ( Value(..), Error(..), ParseError
  , run
  )
  where


import Parser
import Text.Parsec (ParseError)


data Value
  = VNum Integer
  | VBool Bool
  | VNull
  deriving (Eq, Show)


data Error
  = SyntaxError ParseError
  | ExpectedNum Value
  deriving (Eq, Show)


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

    Not a ->
      performNot =<< runExpr a

    Negate a ->
      performNegate =<< runExpr a

    _ ->
      Right VNull


performNot :: Value -> Either Error Value
performNot (VBool b) = Right $ VBool $ not b
performNot VNull = Right $ VBool True
performNot _ = Right $ VBool False


performNegate :: Value -> Either Error Value
performNegate (VNum n) = Right $ VNum $ negate n
performNegate v = Left $ ExpectedNum v
