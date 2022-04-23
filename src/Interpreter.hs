module Interpreter
  ( run
  , Env, Value(..)
  , Error(..), ParseError
  )
  where


import qualified Environment as Env
import qualified Hash
import qualified Runtime

import Control.Monad (join, liftM2)
import Control.Monad.Except (throwError)
import Control.Monad.State.Class (get, gets, modify, put)
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
      fmap (fmap (first RuntimeError)) $ runEval (runProgram program) env

    Left parseError ->
      return (env, Left $ SyntaxError parseError)


runProgram :: Program -> Eval Value
runProgram (Program stmts) = returned <$> runBlock stmts


runBlock :: Block -> Eval Value
runBlock [] = return VNull
runBlock [stmt] = runStmt stmt
runBlock (stmt : rest) = do
  val <- runStmt stmt
  if isReturned val then
    return val
  else
    runBlock rest


runStmt :: Stmt -> Eval Value
runStmt stmt =
  case stmt of
    Let identifier expr ->
      case expr of
        Function _ _ -> do
          modify $ Env.extendRec identifier expr
          return VNull

        _ -> do
          val <- runExpr expr
          modify $ Env.extend identifier val
          return VNull

    Return expr ->
      VReturn <$> runExpr expr

    ExprStmt expr ->
      runExpr expr


runExpr :: Expr -> Eval Value
runExpr expr =
  case expr of
    Var identifier -> do
      env <- get
      case Env.lookup identifier env of
        Env.Value val ->
          return val

        Env.Expr aExpr ->
          runExpr aExpr

        Env.NotFound -> do
          builtinsEnv <- builtins
          case Env.lookup identifier builtinsEnv of
            Env.Value val ->
              return val

            Env.NotFound ->
              throwError $ IdentifierNotFound identifier

            Env.Expr _ ->
              fail "A builtin lookup should never return an expression"

    Num n ->
      return $ VNum n

    Bool b ->
      return $ VBool b

    String s ->
      return $ VString s

    Array exprs ->
      VArray <$> runExprs exprs

    Hash kvs ->
      VHash <$> runKVExprs kvs

    Infix binOp a b ->
      bindM2 (performBinOp binOp) (runExpr a) (runExpr b)

    Prefix unaryOp a ->
      runExpr a >>= performUnaryOp unaryOp

    Call f args ->
      bindM2 callFunction (runExpr f) (runExprs args)

    Index a i -> do
      bindM2 getValueAt (runExpr a) (runExpr i)

    If condition thenBlock maybeElseBlock -> do
      conditionVal <- runExpr condition
      if isTruthy conditionVal then
        runBlock thenBlock
      else
        maybe (return VNull) runBlock maybeElseBlock

    Function params body ->
      gets $ VFunction params body


runExprs :: [Expr] -> Eval [Value]
runExprs = mapM runExpr


runKVExprs :: [(Expr, Expr)] -> Eval Hash
runKVExprs kvExprs = Hash.fromList <$> mapM toKeyVal kvExprs
  where
    toKeyVal (kExpr, vExpr) = do
      key <- runExpr kExpr >>= toKey
      val <- runExpr vExpr
      return (key, val)


performBinOp :: BinOp -> Value -> Value -> Eval Value
performBinOp Equal       = performEqual
performBinOp NotEqual    = performNotEqual
performBinOp LessThan    = performLessThan
performBinOp GreaterThan = performGreaterThan
performBinOp Add         = performAdd
performBinOp Sub         = performSub
performBinOp Mul         = performMul
performBinOp Div         = performDiv


performEqual :: Value -> Value -> Eval Value
performEqual (VNum a) (VNum b) = return $ VBool $ a == b
performEqual (VBool a) (VBool b) = return $ VBool $ a == b
performEqual _ _ = return $ VBool False


performNotEqual :: Value -> Value -> Eval Value
performNotEqual (VNum a) (VNum b) = return $ VBool $ a /= b
performNotEqual (VBool a) (VBool b) = return $ VBool $ a /= b
performNotEqual _ _ = return $ VBool True


performLessThan :: Value -> Value -> Eval Value
performLessThan (VNum a) (VNum b) = return $ VBool $ a < b
performLessThan aVal bVal
  | ta /= tb = throwError $ TypeMismatch $ ta ++ " < " ++ tb
  | otherwise = throwError $ UnknownOperator $ ta ++ " < " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performGreaterThan :: Value -> Value -> Eval Value
performGreaterThan (VNum a) (VNum b) = return $ VBool $ a > b
performGreaterThan aVal bVal
  | ta /= tb = throwError $ TypeMismatch $ ta ++ " > " ++ tb
  | otherwise = throwError $ UnknownOperator $ ta ++ " > " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performAdd :: Value -> Value -> Eval Value
performAdd (VNum a) (VNum b) = return $ VNum $ a + b
performAdd (VString a) (VString b) = return $ VString $ a ++ b
performAdd aVal bVal
  | ta /= tb = throwError $ TypeMismatch $ ta ++ " + " ++ tb
  | otherwise = throwError $ UnknownOperator $ ta ++ " + " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performSub :: Value -> Value -> Eval Value
performSub (VNum a) (VNum b) = return $ VNum $ a - b
performSub aVal bVal
  | ta /= tb = throwError $ TypeMismatch $ ta ++ " - " ++ tb
  | otherwise = throwError $ UnknownOperator $ ta ++ " - " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performMul :: Value -> Value -> Eval Value
performMul (VNum a) (VNum b) = return $ VNum $ a * b
performMul aVal bVal
  | ta /= tb = throwError $ TypeMismatch $ ta ++ " * " ++ tb
  | otherwise = throwError $ UnknownOperator $ ta ++ " * " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performDiv :: Value -> Value -> Eval Value
performDiv (VNum a) (VNum b) = return $ VNum $ a `div` b
performDiv aVal bVal
  | ta /= tb = throwError $ TypeMismatch $ ta ++ " / " ++ tb
  | otherwise = throwError $ UnknownOperator $ ta ++ " / " ++ tb
  where
    ta = typeOf aVal
    tb = typeOf bVal


performUnaryOp :: UnaryOp -> Value -> Eval Value
performUnaryOp Not    = performNot
performUnaryOp Negate = performNegate


performNot :: Value -> Eval Value
performNot (VBool b) = return $ VBool $ not b
performNot VNull = return $ VBool True
performNot _ = return $ VBool False


performNegate :: Value -> Eval Value
performNegate (VNum n) = return $ VNum $ negate n
performNegate v = throwError $ UnknownOperator $ "-" ++ typeOf v


callFunction :: Value -> [Value] -> Eval Value
callFunction (VFunction params body env) args
  | numArgs == numParams = do
    let extendedEnv = Env.extendMany (zip params args) env
    put extendedEnv
    val <- runBlock body
    return $ returned val

  | otherwise = throwError $ ArgumentError $ "wrong number of arguments. got=" ++ show numArgs ++ ", want=" ++ show numParams
  where
    numParams = length params
    numArgs = length args

callFunction (VBuiltinFunction builtin) args = builtin args

callFunction val _ = throwError $ NotAFunction $ typeOf val


getValueAt :: Value -> Value -> Eval Value
getValueAt (VArray arr) (VNum n)
  | n >= 0 && n < genericLength arr = return $ genericIndex arr n
  | otherwise = return VNull

getValueAt (VHash hash) (VNum n) =
  maybe (return VNull) return $ Hash.find (Hash.KNum n) hash

getValueAt (VHash hash) (VBool b) =
  maybe (return VNull) return $ Hash.find (Hash.KBool b) hash

getValueAt (VHash hash) (VString s) =
  maybe (return VNull) return $ Hash.find (Hash.KString s) hash

getValueAt a i = throwError $ TypeMismatch $ typeOf a ++ "[" ++ typeOf i ++ "]"


toKey :: Value -> Eval Hash.Key
toKey (VNum n) = return $ Hash.KNum n
toKey (VBool b) = return $ Hash.KBool b
toKey (VString s) = return $ Hash.KString s
toKey v = throwError $ TypeMismatch $ "unusable as hash key: " ++ typeOf v


-- HELPERS


bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = join $ liftM2 f ma mb
