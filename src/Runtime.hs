module Runtime
  ( Value(..), Env, BuiltinFunction, Error(..)
  , isTruthy, isReturned, returned, typeOf
  , builtins
  ) where


import qualified Environment as Env

import Environment (Environment)
import Parser


data Value
  = VNum Integer
  | VBool Bool
  | VString String
  | VArray [Value]
  | VNull
  | VReturn Value
  | VFunction [Id] Block Env
  | VBuiltinFunction BuiltinFunction

type Env = Environment Id Value

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
typeOf VNull = "NULL"
typeOf (VReturn _) = "RETURN_VALUE"
typeOf (VFunction _ _ _) = "FUNCTION"
typeOf (VBuiltinFunction _) = "BUILTIN"


-- BUILTIN FUNCTIONS


builtins :: IO Env
builtins =
  Env.fromList $ map (fmap VBuiltinFunction)
    [ ("len", builtinLen)
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
