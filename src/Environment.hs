module Environment
  ( Environment
  , empty
  , get
  , set
  , extend
  ) where


import qualified Data.Map as Map


data Environment k v
  = Environment [Map.Map k v]


empty :: Environment k v
empty = Environment [Map.empty]


get :: Ord k => k -> Environment k v -> Maybe v
get k (Environment ms) = helper ms
  where
    helper [] = Nothing
    helper (m:rest) =
      case Map.lookup k m of
        Nothing ->
          helper rest

        success ->
          success


set :: Ord k => k -> v -> Environment k v -> Environment k v
set k v (Environment (m:ms)) = Environment (Map.insert k v m : ms)


extend :: Ord k => [(k, v)] -> Environment k v -> Environment k v
extend bindings (Environment ms) = Environment (Map.fromList bindings : ms)
