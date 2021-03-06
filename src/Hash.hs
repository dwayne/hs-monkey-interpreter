module Hash
  ( Hash, Key(..)
  , fromList
  , find
  , toList
  ) where


import qualified Data.Map as Map


data Hash v = Hash (Map.Map Key v)
  deriving (Eq, Show)


data Key
  = KNum Integer
  | KBool Bool
  | KString String
  deriving (Eq, Ord)


instance Show Key where
  show (KNum n) = show n
  show (KBool b) = show b
  show (KString s) = s


fromList :: [(Key, v)] -> Hash v
fromList = Hash . Map.fromList


find :: Key -> Hash v -> Maybe v
find k (Hash m) = Map.lookup k m


toList :: Hash v -> [(Key, v)]
toList (Hash m) = Map.toList m
