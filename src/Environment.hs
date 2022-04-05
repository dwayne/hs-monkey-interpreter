module Environment
  ( Environment
  , empty
  , get
  , set
  , extend
  ) where


import qualified Data.Map as Map

import Data.IORef (IORef, newIORef, readIORef)


data Environment k v
  = Environment (IORef (Stack k v))


data Stack k v
  = Stack
      { _sTop :: Map.Map k v
      , _sNext :: Maybe (IORef (Stack k v))
      }


empty :: IO (Environment k v)
empty = Environment <$> newIORef stack
  where
    stack = Stack { _sTop = Map.empty, _sNext = Nothing }


get :: Ord k => k -> Environment k v -> IO (Maybe v)
get k (Environment s) = helper s
  where
    helper stackRef = do
      stack <- readIORef stackRef
      case Map.lookup k (_sTop stack) of
        Nothing ->
          case _sNext stack of
            Nothing ->
              return Nothing

            Just nextStackRef ->
              helper nextStackRef

        success ->
          return success


set :: Ord k => k -> v -> Environment k v -> IO (Environment k v)
set k v (Environment stackRef) = do
  stack <- readIORef stackRef
  let m = _sTop stack
  Environment <$> newIORef (stack { _sTop = Map.insert k v m })


extend :: Ord k => [(k, v)] -> Environment k v -> IO (Environment k v)
extend bindings (Environment stackRef) =
  Environment <$> newIORef (
    Stack
      { _sTop = Map.fromList bindings
      , _sNext = Just stackRef
      }
  )
