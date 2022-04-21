module Interpreter
  ( Value(..), Env, Error(..), ParseError
  , run
  )
  where


import qualified Environment as Env
import qualified Hash
import qualified Runtime

import Control.Monad (join, liftM2)
import Data.Bifunctor (first)
import Data.List (genericIndex, genericLength)
import Parser
import Runtime hiding (Error)


data Error
  = SyntaxError ParseError
  | RuntimeError Runtime.Error
  deriving (Eq, Show)


run :: String -> Env -> IO (Env, Either Error Value)
run input env =
  case parse input of
    Right program ->
      fmap (fmap (first RuntimeError)) $ runProgram program env

    Left parseError ->
      return (env, Left $ SyntaxError parseError)


runProgram :: Program -> Env -> IO (Env, Either Runtime.Error Value)
runProgram (Program stmts) env =
  fmap (fmap (fmap returned)) $ runBlock stmts env


runBlock :: Block -> Env -> IO (Env, Either Runtime.Error Value)
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


runStmt :: Stmt -> Env -> IO (Env, Either Runtime.Error Value)
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


runExpr :: Expr -> Env -> IO (Env, Either Runtime.Error Value)
runExpr expr env =
  case expr of
    Var identifier -> do
      maybeVal <- Env.get identifier env
      case maybeVal of
        Just val ->
          return (env, Right val)

        Nothing -> do
          builtinsEnv <- builtins
          maybeBuiltin <- Env.get identifier builtinsEnv
          case maybeBuiltin of
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

    Array exprs -> do
      eitherVals <- runExprs exprs env

      case eitherVals of
        Right vals ->
          return (env, Right $ VArray vals)

        Left err ->
          return (env, Left err)

    Hash kvExprs -> do
      eitherKVS <- runKVExprs kvExprs env

      case eitherKVS of
        Right kvs ->
          return (env, Right $ VHash $ Hash.fromList kvs)

        Left err ->
          return (env, Left err)

    Infix binOp a b -> do
      (env', eitherAVal) <- runExpr a env
      (env'', eitherBVal) <- runExpr b env'

      let result = join $ liftM2 (performBinOp binOp) eitherAVal eitherBVal

      return (env'', result)

    Prefix unaryOp a -> do
      (env', eitherVal) <- runExpr a env
      return (env', eitherVal >>= performUnaryOp unaryOp)

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

    Index aExpr iExpr -> do
      (_, eitherAVal) <- runExpr aExpr env

      case eitherAVal of
        Right aVal -> do
          (_, eitherIVal) <- runExpr iExpr env

          case eitherIVal of
            Right iVal -> do
              return (env, getValueAt aVal iVal)

            Left err ->
              return (env, Left err)

        Left err ->
          return (env, Left err)

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


runExprs :: [Expr] -> Env -> IO (Either Runtime.Error [Value])
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


runKVExprs :: [(Expr, Expr)] -> Env -> IO (Either Runtime.Error [(Hash.Key, Value)])
runKVExprs kvExprs env = helper kvExprs []
  where
    helper [] kvs = return $ Right kvs
    helper ((kExpr, vExpr):rest) kvs = do
      (_, eitherKey) <- runExpr kExpr env
      case eitherKey of
        Right kVal ->
          case toKey kVal of
            Right key -> do
              (_, eitherVal) <- runExpr vExpr env
              case eitherVal of
                Right val ->
                  helper rest ((key, val) : kvs)

                Left err ->
                  return $ Left err

            Left err ->
              return $ Left err

        Left err ->
          return $ Left err


performBinOp :: BinOp -> Value -> Value -> Either Runtime.Error Value
performBinOp Equal       = performEqual
performBinOp NotEqual    = performNotEqual
performBinOp LessThan    = performLessThan
performBinOp GreaterThan = performGreaterThan
performBinOp Add         = performAdd
performBinOp Sub         = performSub
performBinOp Mul         = performMul
performBinOp Div         = performDiv


performEqual :: Value -> Value -> Either Runtime.Error Value
performEqual (VNum a) (VNum b) = Right $ VBool $ a == b
performEqual (VBool a) (VBool b) = Right $ VBool $ a == b
performEqual _ _ = Right $ VBool False


performNotEqual :: Value -> Value -> Either Runtime.Error Value
performNotEqual (VNum a) (VNum b) = Right $ VBool $ a /= b
performNotEqual (VBool a) (VBool b) = Right $ VBool $ a /= b
performNotEqual _ _ = Right $ VBool True


performLessThan :: Value -> Value -> Either Runtime.Error Value
performLessThan (VNum a) (VNum b) = Right $ VBool $ a < b
performLessThan aVal bVal
  | ta /= tb = Left $ TypeMismatch $ ta ++ " < " ++ tb
  | otherwise = Left $ UnknownOperator $ ta ++ " < " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performGreaterThan :: Value -> Value -> Either Runtime.Error Value
performGreaterThan (VNum a) (VNum b) = Right $ VBool $ a > b
performGreaterThan aVal bVal
  | ta /= tb = Left $ TypeMismatch $ ta ++ " > " ++ tb
  | otherwise = Left $ UnknownOperator $ ta ++ " > " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performAdd :: Value -> Value -> Either Runtime.Error Value
performAdd (VNum a) (VNum b) = Right $ VNum $ a + b
performAdd (VString a) (VString b) = Right $ VString $ a ++ b
performAdd aVal bVal
  | ta /= tb = Left $ TypeMismatch $ ta ++ " + " ++ tb
  | otherwise = Left $ UnknownOperator $ ta ++ " + " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performSub :: Value -> Value -> Either Runtime.Error Value
performSub (VNum a) (VNum b) = Right $ VNum $ a - b
performSub aVal bVal
  | ta /= tb = Left $ TypeMismatch $ ta ++ " - " ++ tb
  | otherwise = Left $ UnknownOperator $ ta ++ " - " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performMul :: Value -> Value -> Either Runtime.Error Value
performMul (VNum a) (VNum b) = Right $ VNum $ a * b
performMul aVal bVal
  | ta /= tb = Left $ TypeMismatch $ ta ++ " * " ++ tb
  | otherwise = Left $ UnknownOperator $ ta ++ " * " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performDiv :: Value -> Value -> Either Runtime.Error Value
performDiv (VNum a) (VNum b) = Right $ VNum $ a `div` b
performDiv aVal bVal
  | ta /= tb = Left $ TypeMismatch $ ta ++ " / " ++ tb
  | otherwise = Left $ UnknownOperator $ ta ++ " / " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performUnaryOp :: UnaryOp -> Value -> Either Runtime.Error Value
performUnaryOp Not    = performNot
performUnaryOp Negate = performNegate


performNot :: Value -> Either Runtime.Error Value
performNot (VBool b) = Right $ VBool $ not b
performNot VNull = Right $ VBool True
performNot _ = Right $ VBool False


performNegate :: Value -> Either Runtime.Error Value
performNegate (VNum n) = Right $ VNum $ negate n
performNegate v = Left $ UnknownOperator $ "-" ++ typeOf v


callFunction :: Value -> [Value] -> IO (Either Runtime.Error Value)
callFunction (VFunction params body env) args
  | numArgs == numParams = do
    extendedEnv <- Env.extend (zip params args) env
    (_, eitherVal) <- runBlock body extendedEnv
    case eitherVal of
      Right val ->
        return $ Right $ returned val

      err ->
        return err
  | otherwise = return $ Left $ ArgumentError $ "wrong number of arguments. got=" ++ show numArgs ++ ", want=" ++ show numParams
  where
    numParams = length params
    numArgs = length args

callFunction (VBuiltinFunction builtin) args = builtin args

callFunction val _ = return $ Left $ NotAFunction $ typeOf val


getValueAt :: Value -> Value -> Either Runtime.Error Value
getValueAt (VArray arr) (VNum n)
  | n >= 0 && n < genericLength arr = Right $ genericIndex arr n
  | otherwise = Right VNull

getValueAt (VHash hash) (VNum n) =
  maybe (Right VNull) Right $ Hash.find (Hash.KNum n) hash

getValueAt (VHash hash) (VBool b) =
  maybe (Right VNull) Right $ Hash.find (Hash.KBool b) hash

getValueAt (VHash hash) (VString s) =
  maybe (Right VNull) Right $ Hash.find (Hash.KString s) hash

getValueAt a i = Left $ TypeMismatch $ typeOf a ++ "[" ++ typeOf i ++ "]"


toKey :: Value -> Either Runtime.Error Hash.Key
toKey (VNum n) = Right $ Hash.KNum n
toKey (VBool b) = Right $ Hash.KBool b
toKey (VString s) = Right $ Hash.KString s
toKey v = Left $ TypeMismatch $ "unusable as hash key: " ++ typeOf v
