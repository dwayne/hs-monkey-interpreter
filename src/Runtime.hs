{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Runtime
  ( Eval
  , runEval

  , Value(..)
  , Env, Hash, BuiltinFunction
  , Error(..)

  , isTruthy, isReturned, returned, typeOf

  , builtins
  ) where


import qualified Environment as Env
import qualified Hash

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Monad.State.Class (MonadState, get, put)
import Environment (Environment)
import Parser


-- EVAL


newtype Eval a
  = Eval { runEval :: Env -> IO (Env, Either Error a) }

type Env = Environment Id Value Expr

data Error
  = TypeMismatch String
  | UnknownOperator String
  | IdentifierNotFound String
  | NotAFunction String
  | ArgumentError String
  | BuiltinError String
  | UnexpectedError String
  deriving (Eq, Show)


instance Functor Eval where
  fmap f (Eval t) = Eval $ \env -> fmap (fmap (fmap f)) (t env)


instance Applicative Eval where
  pure a = Eval $ \env -> pure (env, pure a)

  (Eval tab) <*> (Eval ta) =
    Eval $ \env -> do
      (env', eitherF) <- tab env
      case eitherF of
        Right f -> fmap (fmap (fmap f)) (ta env')
        Left e  -> return (env', Left e)


instance Monad Eval where
  (Eval t) >>= f =
    Eval $ \env -> do
      (env', eitherA) <- t env
      case eitherA of
        Right a -> runEval (f a) env'
        Left e  -> return (env', Left e)


instance MonadFail Eval where
  fail s = Eval $ \env -> return (env, Left $ UnexpectedError s)


instance MonadState Env Eval where
  get = Eval $ \env -> return (env, return env)
  put env = Eval $ \_ -> return (env, return ())


instance MonadError Error Eval where
  throwError err = Eval $ \env -> return (env, Left err)
  (Eval t) `catchError` handler =
    Eval $ \env -> do
      (env', eitherA) <- t env
      case eitherA of
        Right a -> return (env', Right a)
        Left e  -> runEval (handler e) env'


instance MonadIO Eval where
  liftIO action =
    Eval $ \env -> do
      a <- action
      return (env, return a)


-- VALUE


data Value
  = VNum Integer
  | VBool Bool
  | VString String
  | VArray [Value]
  | VHash Hash
  | VNull
  | VReturn Value
  | VFunction [Id] Block Env
  | VBuiltinFunction BuiltinFunction

type Hash = Hash.Hash Value
type BuiltinFunction = [Value] -> Eval Value


instance Eq Value where
  VNum a == VNum b = a == b
  VBool a == VBool b = a == b
  VString a == VString b = a == b
  VArray a == VArray b = a == b
  VHash a == VHash b = a == b
  VNull == VNull = True

  VReturn aVal == bVal = aVal == bVal
  aVal == VReturn bVal = aVal == bVal

  _ == _ = False


instance Show Value where
  show (VNum n) = show n

  show (VBool True) = "true"
  show (VBool False) = "false"

  show (VString s) = s

  show (VArray a) =
    let
      showValues [] = ""
      showValues [v] = show v
      showValues (v:vs) = show v ++ ", " ++ showValues vs
    in
    "[" ++ showValues a ++ "]"

  show (VHash h) =
    let
      showKeyValue (k, v) = show k ++ ": " ++ show v
      showKeyValues [] = ""
      showKeyValues [kv] = showKeyValue kv
      showKeyValues (kv:kvs) = showKeyValue kv ++ ", " ++ showKeyValues kvs
    in
    "{" ++ showKeyValues (Hash.toList h) ++ "}"

  show VNull = "null"

  show (VReturn val) = show val

  show (VFunction _ _ _) = "<function>"
  show (VBuiltinFunction _) = "<builtin function>"


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
typeOf (VArray _) = "ARRAY"
typeOf (VHash _) = "HASH"
typeOf VNull = "NULL"
typeOf (VReturn _) = "RETURN_VALUE"
typeOf (VFunction _ _ _) = "FUNCTION"
typeOf (VBuiltinFunction _) = "BUILTIN"


-- BUILTIN FUNCTIONS


builtins :: Eval Env
builtins =
  return $ Env.fromList $ map (fmap VBuiltinFunction)
    [ ("len", builtinLen)
    , ("first", builtinFirst)
    , ("last", builtinLast)
    , ("rest", builtinRest)
    , ("push", builtinPush)
    , ("puts", builtinPuts)
    ]


builtinLen :: BuiltinFunction
builtinLen args =
  case args of
    [VString s] ->
      return $ VNum $ fromIntegral $ length s

    [VArray a] ->
      return $ VNum $ fromIntegral $ length a

    [arg] ->
      throwError $ BuiltinError $ "argument to `len` not supported, got " ++ typeOf arg

    _ ->
      throwError $ BuiltinError $ "wrong number of arguments. got=" ++ show (length args) ++ ", want=1"


builtinFirst :: BuiltinFunction
builtinFirst args =
  case args of
    [VArray []] ->
      return VNull

    [VArray (x:_)] ->
      return x

    [arg] ->
      throwError $ BuiltinError $ "argument to `first` must be ARRAY, got " ++ typeOf arg

    _ ->
      throwError $ BuiltinError $ "wrong number of arguments. got=" ++ show (length args) ++ ", want=1"


builtinLast :: BuiltinFunction
builtinLast args =
  case args of
    [VArray []] ->
      return VNull

    [VArray arr] ->
      return $ last arr

    [arg] ->
      throwError $ BuiltinError $ "argument to `last` must be ARRAY, got " ++ typeOf arg

    _ ->
      throwError $ BuiltinError $ "wrong number of arguments. got=" ++ show (length args) ++ ", want=1"


builtinRest :: BuiltinFunction
builtinRest args =
  case args of
    [VArray []] ->
      return VNull

    [VArray (_:rest)] ->
      return $ VArray rest

    [arg] ->
      throwError $ BuiltinError $ "argument to `rest` must be ARRAY, got " ++ typeOf arg

    _ ->
      throwError $ BuiltinError $ "wrong number of arguments. got=" ++ show (length args) ++ ", want=1"


builtinPush :: BuiltinFunction
builtinPush args =
  case args of
    [VArray arr, val] ->
      return $ VArray $ arr ++ [val]

    [subject, _] ->
      throwError $ BuiltinError $ "argument to `push` must be ARRAY, got " ++ typeOf subject

    _ ->
      throwError $ BuiltinError $ "wrong number of arguments. got=" ++ show (length args) ++ ", want=2"


builtinPuts :: BuiltinFunction
builtinPuts args =
  case args of
    [] ->
      return VNull

    (arg:rest) ->
      liftIO (print arg) >> builtinPuts rest

  -- Alternatively, we can derive the following:
  --
  -- > liftIO (mapM_ print args) >> return VNull
  --
  -- Is that more readable?
