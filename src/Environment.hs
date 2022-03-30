module Environment
  ( Environment
  , empty
  , get
  , set
  ) where


import qualified Data.Map as Map


data Environment k v
  = Environment (Map.Map k v)


empty :: Environment k v
empty = Environment $ Map.empty


get :: Ord k => k -> Environment k v -> Maybe v
get k (Environment m) = Map.lookup k m


set :: Ord k => k -> v -> Environment k v -> Environment k v
set k v (Environment m) = Environment $ Map.insert k v m
