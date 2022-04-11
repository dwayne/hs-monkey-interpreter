module Runtime
  ( Value(..), Env, Hash, BuiltinFunction, Error(..)
  , isTruthy, isReturned, returned, typeOf
  , builtins
  ) where


import qualified Environment as Env
import qualified Hash

import Environment (Environment)
import Parser


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

type Env = Environment Id Value
type Hash = Hash.Hash Value

type BuiltinFunction = [Value] -> Either Error Value

data Error
  = TypeMismatch String
  | UnknownOperator String
  | IdentifierNotFound String
  | NotAFunction String
  | BuiltinError String
  deriving (Eq, Show)


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


builtins :: IO Env
builtins =
  Env.fromList $ map (fmap VBuiltinFunction)
    [ ("len", builtinLen)
    , ("first", builtinFirst)
    , ("last", builtinLast)
    , ("rest", builtinRest)
    , ("push", builtinPush)
    ]


builtinLen :: BuiltinFunction
builtinLen args =
  case args of
    [VString s] ->
      Right $ VNum $ fromIntegral $ length s

    [VArray a] ->
      Right $ VNum $ fromIntegral $ length a

    [arg] ->
      Left $ BuiltinError $ "argument to `len` not supported, got " ++ typeOf arg

    _ ->
      Left $ BuiltinError $ "wrong number of arguments. got=" ++ show (length args) ++ ", want=1"


builtinFirst :: BuiltinFunction
builtinFirst args =
  case args of
    [VArray []] ->
      Right VNull

    [VArray (x:_)] ->
      Right x

    [arg] ->
      Left $ BuiltinError $ "argument to `first` must be ARRAY, got " ++ typeOf arg

    _ ->
      Left $ BuiltinError $ "wrong number of arguments. got=" ++ show (length args) ++ ", want=1"


builtinLast :: BuiltinFunction
builtinLast args =
  case args of
    [VArray []] ->
      Right VNull

    [VArray arr] ->
      Right $ last arr

    [arg] ->
      Left $ BuiltinError $ "argument to `last` must be ARRAY, got " ++ typeOf arg

    _ ->
      Left $ BuiltinError $ "wrong number of arguments. got=" ++ show (length args) ++ ", want=1"


builtinRest :: BuiltinFunction
builtinRest args =
  case args of
    [VArray []] ->
      Right VNull

    [VArray (_:rest)] ->
      Right $ VArray rest

    [arg] ->
      Left $ BuiltinError $ "argument to `rest` must be ARRAY, got " ++ typeOf arg

    _ ->
      Left $ BuiltinError $ "wrong number of arguments. got=" ++ show (length args) ++ ", want=1"


builtinPush :: BuiltinFunction
builtinPush args =
  case args of
    [VArray arr, val] ->
      Right $ VArray $ arr ++ [val]

    [subject, _] ->
      Left $ BuiltinError $ "argument to `push` must be ARRAY, got " ++ typeOf subject

    _ ->
      Left $ BuiltinError $ "wrong number of arguments. got=" ++ show (length args) ++ ", want=2"
