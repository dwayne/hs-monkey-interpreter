module Interpreter
  ( Value(..), Env, Error(..), ParseError
  , run
  )
  where


import qualified Environment as Env

import Environment (Environment)
import Parser
import Text.Parsec (ParseError)


data Value
  = VNum Integer
  | VBool Bool
  | VNull
  | VReturn Value
  | VFunction [Id] Block Env


type Env = Environment Id Value


instance Eq Value where
  VNum a == VNum b = a == b
  VBool a == VBool b = a == b
  VNull == VNull = True

  VReturn aVal == bVal = aVal == bVal
  aVal == VReturn bVal = aVal == bVal

  _ == _ = False


instance Show Value where
  show (VNum n) = show n

  show (VBool True) = "true"
  show (VBool False) = "false"

  show VNull = "null"

  show (VReturn val) = show val

  show (VFunction _ _ _) = "<function>"


data Error
  = SyntaxError ParseError
  | TypeMismatch String
  | UnknownOperator String
  | IdentifierNotFound String
  | NotAFunction String
  deriving (Eq, Show)


run :: String -> Env -> (Env, Either Error Value)
run input env =
  case parse input of
    Right program ->
      runProgram program env

    Left parseError ->
      (env, Left $ SyntaxError parseError)


runProgram :: Program -> Env -> (Env, Either Error Value)
runProgram (Program stmts) env =
  fmap (fmap returned) $ runBlock stmts env


runBlock :: Block -> Env -> (Env, Either Error Value)
runBlock [] env = (env, Right VNull)
runBlock [stmt] env = runStmt stmt env
runBlock (stmt : rest) env =
  let
    (env', eitherVal) =
      runStmt stmt env
  in
  case eitherVal of
    Right val ->
      if isReturned val then
        (env', Right val)
      else
        runBlock rest env'

    Left err ->
      (env', Left err)


runStmt :: Stmt -> Env -> (Env, Either Error Value)
runStmt stmt env =
  case stmt of
    Let identifier expr ->
      let
        (env', eitherVal) =
          runExpr expr env
      in
      case eitherVal of
        Right val ->
          (Env.set identifier val env', Right VNull)

        Left err ->
          (env, Left err)

    Return expr ->
      fmap (fmap VReturn) $ runExpr expr env

    ExprStmt expr ->
      runExpr expr env


runExpr :: Expr -> Env -> (Env, Either Error Value)
runExpr expr env =
  case expr of
    Var identifier ->
      ( env
      , case Env.get identifier env of
          Just val ->
            Right val

          Nothing ->
            Left $ IdentifierNotFound identifier
      )

    Num n ->
      (env, Right $ VNum n)

    Bool b ->
      (env, Right $ VBool b)

    Not a ->
      let
        (env', eitherVal) =
          runExpr a env
      in
      (env', eitherVal >>= performNot)

    Negate a ->
      let
        (env', eitherVal) =
          runExpr a env
      in
      (env', eitherVal >>= performNegate)

    Add a b ->
      let
        (env', eitherAVal) =
          runExpr a env

        (env'', eitherBVal) =
          runExpr b env'

        result = do
          aVal <- eitherAVal
          bVal <- eitherBVal
          performAdd aVal bVal
      in
      (env'', result)

    Sub a b ->
      let
        (env', eitherAVal) =
          runExpr a env

        (env'', eitherBVal) =
          runExpr b env'

        result = do
          aVal <- eitherAVal
          bVal <- eitherBVal
          performSub aVal bVal
      in
      (env'', result)

    Mul a b ->
      let
        (env', eitherAVal) =
          runExpr a env

        (env'', eitherBVal) =
          runExpr b env'

        result = do
          aVal <- eitherAVal
          bVal <- eitherBVal
          performMul aVal bVal
      in
      (env'', result)

    Div a b ->
      let
        (env', eitherAVal) =
          runExpr a env

        (env'', eitherBVal) =
          runExpr b env'

        result = do
          aVal <- eitherAVal
          bVal <- eitherBVal
          performDiv aVal bVal
      in
      (env'', result)

    LessThan a b ->
      let
        (env', eitherAVal) =
          runExpr a env

        (env'', eitherBVal) =
          runExpr b env'

        result = do
          aVal <- eitherAVal
          bVal <- eitherBVal
          performLessThan aVal bVal
      in
      (env'', result)

    GreaterThan a b ->
      let
        (env', eitherAVal) =
          runExpr a env

        (env'', eitherBVal) =
          runExpr b env'

        result = do
          aVal <- eitherAVal
          bVal <- eitherBVal
          performGreaterThan aVal bVal
      in
      (env'', result)

    Equal a b ->
      let
        (env', eitherAVal) =
          runExpr a env

        (env'', eitherBVal) =
          runExpr b env'

        result = do
          aVal <- eitherAVal
          bVal <- eitherBVal
          performEqual aVal bVal
      in
      (env'', result)

    NotEqual a b ->
      let
        (env', eitherAVal) =
          runExpr a env

        (env'', eitherBVal) =
          runExpr b env'

        result = do
          aVal <- eitherAVal
          bVal <- eitherBVal
          performNotEqual aVal bVal
      in
      (env'', result)

    If condition thenBlock maybeElseBlock ->
      let
        (env', eitherConditionVal) =
          runExpr condition env
      in
      case eitherConditionVal of
        Right conditionVal ->
          if isTruthy conditionVal then
            runBlock thenBlock env'
          else
            case maybeElseBlock of
              Nothing ->
                (env', Right VNull)

              Just elseBlock ->
                runBlock elseBlock env'

        Left err ->
          (env', Left err)

    Function params body ->
      (env, Right $ VFunction params body env)

    Call fExpr argExprs ->
      let
        eitherFVal =
          snd $ runExpr fExpr env

        eitherArgVals =
          runExprs argExprs env
      in
      case eitherFVal of
        Right fVal ->
          case eitherArgVals of
            Right argVals ->
              (env, callFunction fVal argVals)

            Left err ->
              (env, Left err)

        Left err ->
          (env, Left err)


runExprs :: [Expr] -> Env -> Either Error [Value]
runExprs exprs env = helper exprs []
  where
    helper [] vals = Right $ reverse vals
    helper (expr:rest) vals =
      case snd (runExpr expr env) of
        Right val ->
          helper rest (val : vals)

        Left err ->
          Left err


performNot :: Value -> Either Error Value
performNot (VBool b) = Right $ VBool $ not b
performNot VNull = Right $ VBool True
performNot _ = Right $ VBool False


performNegate :: Value -> Either Error Value
performNegate (VNum n) = Right $ VNum $ negate n
performNegate v = Left $ UnknownOperator $ "-" ++ typeOf v


performAdd :: Value -> Value -> Either Error Value
performAdd (VNum a) (VNum b) = Right $ VNum $ a + b
performAdd aVal bVal
  | ta /= tb = Left $ TypeMismatch $ ta ++ " + " ++ tb
  | otherwise = Left $ UnknownOperator $ ta ++ " + " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performSub :: Value -> Value -> Either Error Value
performSub (VNum a) (VNum b) = Right $ VNum $ a - b
performSub aVal bVal
  | ta /= tb = Left $ TypeMismatch $ ta ++ " - " ++ tb
  | otherwise = Left $ UnknownOperator $ ta ++ " - " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performMul :: Value -> Value -> Either Error Value
performMul (VNum a) (VNum b) = Right $ VNum $ a * b
performMul aVal bVal
  | ta /= tb = Left $ TypeMismatch $ ta ++ " * " ++ tb
  | otherwise = Left $ UnknownOperator $ ta ++ " * " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performDiv :: Value -> Value -> Either Error Value
performDiv (VNum a) (VNum b) = Right $ VNum $ a `div` b
performDiv aVal bVal
  | ta /= tb = Left $ TypeMismatch $ ta ++ " / " ++ tb
  | otherwise = Left $ UnknownOperator $ ta ++ " / " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performLessThan :: Value -> Value -> Either Error Value
performLessThan (VNum a) (VNum b) = Right $ VBool $ a < b
performLessThan aVal bVal
  | ta /= tb = Left $ TypeMismatch $ ta ++ " < " ++ tb
  | otherwise = Left $ UnknownOperator $ ta ++ " < " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performGreaterThan :: Value -> Value -> Either Error Value
performGreaterThan (VNum a) (VNum b) = Right $ VBool $ a > b
performGreaterThan aVal bVal
  | ta /= tb = Left $ TypeMismatch $ ta ++ " > " ++ tb
  | otherwise = Left $ UnknownOperator $ ta ++ " > " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performEqual :: Value -> Value -> Either Error Value
performEqual (VNum a) (VNum b) = Right $ VBool $ a == b
performEqual (VBool a) (VBool b) = Right $ VBool $ a == b
performEqual _ _ = Right $ VBool False


performNotEqual :: Value -> Value -> Either Error Value
performNotEqual (VNum a) (VNum b) = Right $ VBool $ a /= b
performNotEqual (VBool a) (VBool b) = Right $ VBool $ a /= b
performNotEqual _ _ = Right $ VBool True


callFunction :: Value -> [Value] -> Either Error Value
callFunction (VFunction params body env) args =
  let
    extendedEnv =
      Env.extend (zip params args) env
  in
  case snd (runBlock body extendedEnv) of
    Right val ->
      Right $ returned val

    err ->
      err

callFunction val _ = Left $ NotAFunction $ typeOf val


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


typeOf :: Value -> String
typeOf (VNum _) = "INTEGER"
typeOf (VBool _) = "BOOLEAN"
typeOf VNull = "NULL"
typeOf (VReturn _) = "RETURN_VALUE"
typeOf (VFunction _ _ _) = "FUNCTION"
