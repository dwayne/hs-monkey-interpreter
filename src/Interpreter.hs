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

    Add a b -> do
      aVal <- runExpr a
      bVal <- runExpr b
      performAdd aVal bVal

    Sub a b -> do
      aVal <- runExpr a
      bVal <- runExpr b
      performSub aVal bVal

    Mul a b -> do
      aVal <- runExpr a
      bVal <- runExpr b
      performMul aVal bVal

    Div a b -> do
      aVal <- runExpr a
      bVal <- runExpr b
      performDiv aVal bVal

    LessThan a b -> do
      aVal <- runExpr a
      bVal <- runExpr b
      performLessThan aVal bVal

    GreaterThan a b -> do
      aVal <- runExpr a
      bVal <- runExpr b
      performGreaterThan aVal bVal

    Equal a b -> do
      aVal <- runExpr a
      bVal <- runExpr b
      performEqual aVal bVal

    NotEqual a b -> do
      aVal <- runExpr a
      bVal <- runExpr b
      performNotEqual aVal bVal

    _ ->
      Right VNull


performNot :: Value -> Either Error Value
performNot (VBool b) = Right $ VBool $ not b
performNot VNull = Right $ VBool True
performNot _ = Right $ VBool False


performNegate :: Value -> Either Error Value
performNegate (VNum n) = Right $ VNum $ negate n
performNegate v = Left $ ExpectedNum v


performAdd :: Value -> Value -> Either Error Value
performAdd (VNum a) (VNum b) = Right $ VNum $ a + b
performAdd _ _ = Right $ VNull


performSub :: Value -> Value -> Either Error Value
performSub (VNum a) (VNum b) = Right $ VNum $ a - b
performSub _ _ = Right $ VNull


performMul :: Value -> Value -> Either Error Value
performMul (VNum a) (VNum b) = Right $ VNum $ a * b
performMul _ _ = Right $ VNull


performDiv :: Value -> Value -> Either Error Value
performDiv (VNum a) (VNum b) = Right $ VNum $ a `div` b
performDiv _ _ = Right $ VNull


performLessThan :: Value -> Value -> Either Error Value
performLessThan (VNum a) (VNum b) = Right $ VBool $ a < b
performLessThan _ _ = Right $ VNull


performGreaterThan :: Value -> Value -> Either Error Value
performGreaterThan (VNum a) (VNum b) = Right $ VBool $ a > b
performGreaterThan _ _ = Right $ VNull


performEqual :: Value -> Value -> Either Error Value
performEqual (VNum a) (VNum b) = Right $ VBool $ a == b
performEqual _ _ = Right $ VNull


performNotEqual :: Value -> Value -> Either Error Value
performNotEqual (VNum a) (VNum b) = Right $ VBool $ a /= b
performNotEqual _ _ = Right $ VNull
