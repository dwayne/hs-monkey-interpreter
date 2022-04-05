module Environment
  ( Environment
  , empty
  , get
  , set
  , extend
  ) where


import qualified Data.Map as Map

import Data.IORef (IORef, newIORef, readIORef, writeIORef)


data Environment k v
  = Environment (IORef (Stack k v))


data Stack k v
  = Stack
      { _sTop :: Map.Map k v
      , _sNext :: Maybe (Stack k v)
      }


empty :: IO (Environment k v)
empty = Environment <$> newIORef stack
  where
    stack = Stack { _sTop = Map.empty, _sNext = Nothing }


get :: Ord k => k -> Environment k v -> IO (Maybe v)
get k (Environment stackRef) = helper <$> readIORef stackRef
  where
    helper stack = do
      case Map.lookup k (_sTop stack) of
        Nothing ->
          helper =<< _sNext stack

        success ->
          success


set :: Ord k => k -> v -> Environment k v -> IO ()
set k v (Environment stackRef) = do
  stack <- readIORef stackRef
  let m = _sTop stack
  writeIORef stackRef $ stack { _sTop = Map.insert k v m }


extend :: Ord k => [(k, v)] -> Environment k v -> IO (Environment k v)
extend bindings (Environment stackRef) = do
  stack <- readIORef stackRef
  let newStack = Stack { _sTop = Map.fromList bindings, _sNext = Just stack }
  Environment <$> newIORef newStack
