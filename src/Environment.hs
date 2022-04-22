module Environment
  ( Environment
  , empty, fromList

  , Result(..)
  , lookup

  , extend, extendRec, extendMany
  ) where


import qualified Data.Map as Map

import Prelude hiding (lookup)


data Environment k v e
  = Empty
  | Single k v (Environment k v e)
  | SingleRec k e (Environment k v e)
  | Multi (Map.Map k v) (Environment k v e)


empty :: Environment k v e
empty = Empty


fromList :: Ord k => [(k, v)] -> Environment k v e
fromList bindings = Multi (Map.fromList bindings) Empty


data Result k v e
  = NotFound
  | Value v
  | Thunk e (Environment k v e)


lookup :: Ord k => k -> Environment k v e -> Result k v e
lookup searchKey env =
  case env of
    Empty ->
      NotFound

    Single k v nextEnv ->
      if searchKey == k then
        Value v
      else
        lookup searchKey nextEnv

    SingleRec k e nextEnv ->
      if searchKey == k then
        Thunk e env
      else
        lookup searchKey nextEnv

    Multi bindings nextEnv ->
      case Map.lookup searchKey bindings of
        Nothing ->
          lookup searchKey nextEnv

        Just v ->
          Value v


extend :: k -> v -> Environment k v e -> Environment k v e
extend k v env = Single k v env


extendRec :: k -> e -> Environment k v e -> Environment k v e
extendRec k e env = SingleRec k e env


extendMany :: Ord k => [(k, v)] -> Environment k v e -> Environment k v e
extendMany bindings env = Multi (Map.fromList bindings) env
