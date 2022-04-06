module Interpreter
  ( Value(..), Env, Error(..), ParseError
  , run
  )
  where


import qualified Environment as Env

import Control.Monad (join, liftM2)
import Environment (Environment)
import Parser
import Text.Parsec (ParseError)


data Value
  = VNum Integer
  | VBool Bool
  | VString String
  | VNull
  | VReturn Value
  | VFunction [Id] Block Env


type Env = Environment Id Value


instance Eq Value where
  VNum a == VNum b = a == b
  VBool a == VBool b = a == b
  VString a == VString b = a == b
  VNull == VNull = True

  VReturn aVal == bVal = aVal == bVal
  aVal == VReturn bVal = aVal == bVal

  _ == _ = False


instance Show Value where
  show (VNum n) = show n

  show (VBool True) = "true"
  show (VBool False) = "false"

  show (VString s) = s

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


run :: String -> Env -> IO (Env, Either Error Value)
run input env =
  case parse input of
    Right program ->
      runProgram program env

    Left parseError ->
      return (env, Left $ SyntaxError parseError)


runProgram :: Program -> Env -> IO (Env, Either Error Value)
runProgram (Program stmts) env =
  fmap (fmap (fmap returned)) $ runBlock stmts env


runBlock :: Block -> Env -> IO (Env, Either Error Value)
runBlock [] env = return (env, Right VNull)
runBlock [stmt] env = runStmt stmt env
runBlock (stmt : rest) env = do
  (env', eitherVal) <- runStmt stmt env

  case eitherVal of
    Right val ->
      if isReturned val then
        return (env', Right val)
      else
        runBlock rest env'

    Left err ->
      return (env', Left err)


runStmt :: Stmt -> Env -> IO (Env, Either Error Value)
runStmt stmt env =
  case stmt of
    Let identifier expr -> do
      (env', eitherVal) <- runExpr expr env

      case eitherVal of
        Right val -> do
          Env.set identifier val env'
          return (env', Right VNull)

        Left err ->
          return (env, Left err)

    Return expr ->
      fmap (fmap (fmap VReturn)) $ runExpr expr env

    ExprStmt expr ->
      runExpr expr env


runExpr :: Expr -> Env -> IO (Env, Either Error Value)
runExpr expr env =
  case expr of
    Var identifier -> do
      maybeVal <- Env.get identifier env
      case maybeVal of
        Just val ->
          return (env, Right val)

        Nothing ->
          return (env, Left $ IdentifierNotFound identifier)

    Num n ->
      return (env, Right $ VNum n)

    Bool b ->
      return (env, Right $ VBool b)

    String s ->
      return (env, Right $ VString s)

    Not a -> do
      (env', eitherVal) <- runExpr a env
      return (env', eitherVal >>= performNot)

    Negate a -> do
      (env', eitherVal) <- runExpr a env
      return (env', eitherVal >>= performNegate)

    Add a b -> do
      (env', eitherAVal) <- runExpr a env
      (env'', eitherBVal) <- runExpr b env'

      let result = join $ liftM2 performAdd eitherAVal eitherBVal

      return (env'', result)

    Sub a b -> do
      (env', eitherAVal) <- runExpr a env
      (env'', eitherBVal) <- runExpr b env'

      let result = join $ liftM2 performSub eitherAVal eitherBVal

      return (env'', result)

    Mul a b -> do
      (env', eitherAVal) <- runExpr a env
      (env'', eitherBVal) <- runExpr b env'

      let result = join $ liftM2 performMul eitherAVal eitherBVal

      return (env'', result)

    Div a b -> do
      (env', eitherAVal) <- runExpr a env
      (env'', eitherBVal) <- runExpr b env'

      let result = join $ liftM2 performDiv eitherAVal eitherBVal

      return (env'', result)

    LessThan a b -> do
      (env', eitherAVal) <- runExpr a env
      (env'', eitherBVal) <- runExpr b env'

      let result = join $ liftM2 performLessThan eitherAVal eitherBVal

      return (env'', result)

    GreaterThan a b -> do
      (env', eitherAVal) <- runExpr a env
      (env'', eitherBVal) <- runExpr b env'

      let result = join $ liftM2 performGreaterThan eitherAVal eitherBVal

      return (env'', result)

    Equal a b -> do
      (env', eitherAVal) <- runExpr a env
      (env'', eitherBVal) <- runExpr b env'

      let result = join $ liftM2 performEqual eitherAVal eitherBVal

      return (env'', result)

    NotEqual a b -> do
      (env', eitherAVal) <- runExpr a env
      (env'', eitherBVal) <- runExpr b env'

      let result = join $ liftM2 performNotEqual eitherAVal eitherBVal

      return (env'', result)

    If condition thenBlock maybeElseBlock -> do
      (env', eitherConditionVal) <- runExpr condition env
      case eitherConditionVal of
        Right conditionVal ->
          if isTruthy conditionVal then
            runBlock thenBlock env'
          else
            case maybeElseBlock of
              Nothing ->
                return (env', Right VNull)

              Just elseBlock ->
                runBlock elseBlock env'

        Left err ->
          return (env', Left err)

    Function params body ->
      return (env, Right $ VFunction params body env)

    Call fExpr argExprs -> do
      (_, eitherFVal) <- runExpr fExpr env
      eitherArgVals <- runExprs argExprs env

      case eitherFVal of
        Right fVal ->
          case eitherArgVals of
            Right argVals -> do
              eitherVal <- callFunction fVal argVals
              return (env, eitherVal)

            Left err ->
              return (env, Left err)

        Left err ->
          return (env, Left err)


runExprs :: [Expr] -> Env -> IO (Either Error [Value])
runExprs exprs env = helper exprs []
  where
    helper [] vals = return $ Right $ reverse vals
    helper (expr:rest) vals = do
      (_, eitherVal) <- runExpr expr env
      case eitherVal of
        Right val ->
          helper rest (val : vals)

        Left err ->
          return $ Left err


performNot :: Value -> Either Error Value
performNot (VBool b) = Right $ VBool $ not b
performNot VNull = Right $ VBool True
performNot _ = Right $ VBool False


performNegate :: Value -> Either Error Value
performNegate (VNum n) = Right $ VNum $ negate n
performNegate v = Left $ UnknownOperator $ "-" ++ typeOf v


performAdd :: Value -> Value -> Either Error Value
performAdd (VNum a) (VNum b) = Right $ VNum $ a + b
performAdd (VString a) (VString b) = Right $ VString $ a ++ b
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


callFunction :: Value -> [Value] -> IO (Either Error Value)
callFunction (VFunction params body env) args = do
  extendedEnv <- Env.extend (zip params args) env
  (_, eitherVal) <- runBlock body extendedEnv
  case eitherVal of
    Right val ->
      return $ Right $ returned val

    err ->
      return err

callFunction val _ = return $ Left $ NotAFunction $ typeOf val


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
typeOf (VString _) = "STRING"
typeOf VNull = "NULL"
typeOf (VReturn _) = "RETURN_VALUE"
typeOf (VFunction _ _ _) = "FUNCTION"
