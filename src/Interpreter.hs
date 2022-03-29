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
  | VReturn Value
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
runProgram (Program stmts) = returned <$> runBlock stmts


runBlock :: [Stmt] -> Either Error Value
runBlock [] = Right VNull
runBlock [stmt] = runStmt stmt
runBlock (stmt : rest) = do
  val <- runStmt stmt
  if isReturned val
    then Right val
    else runBlock rest


runStmt :: Stmt -> Either Error Value
runStmt stmt =
  case stmt of
    Let _ _ ->
      Right VNull

    Return expr ->
      VReturn <$> runExpr expr

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

    If condition thenBlock maybeElseBlock -> do
      conditionVal <- runExpr condition
      if isTruthy conditionVal
        then runBlock thenBlock
        else
          case maybeElseBlock of
            Nothing ->
              Right VNull

            Just elseBlock ->
              runBlock elseBlock

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
performEqual (VBool a) (VBool b) = Right $ VBool $ a == b
performEqual _ _ = Right $ VNull


performNotEqual :: Value -> Value -> Either Error Value
performNotEqual (VNum a) (VNum b) = Right $ VBool $ a /= b
performNotEqual (VBool a) (VBool b) = Right $ VBool $ a /= b
performNotEqual _ _ = Right $ VNull


isTruthy :: Value -> Bool
isTruthy VNull = False
isTruthy (VBool False) = False
isTruthy _ = True


isReturned :: Value -> Bool
isReturned (VReturn _) = True
isReturned _ = False


returned :: Value -> Value
returned (VReturn val) = returned val
returned val = val
