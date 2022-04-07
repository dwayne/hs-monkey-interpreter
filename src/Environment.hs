module Environment
  ( Environment
  , empty, fromList
  , get
  , set
  , extend
  ) where


import qualified Data.Map as Map

import Data.IORef (IORef, newIORef, readIORef, writeIORef)


data Environment k v
  = Environment (IORef [Map.Map k v])


empty :: IO (Environment k v)
empty = Environment <$> newIORef [Map.empty]


fromList :: Ord k => [(k, v)] -> IO (Environment k v)
fromList bindings = Environment <$> newIORef [Map.fromList bindings]


get :: Ord k => k -> Environment k v -> IO (Maybe v)
get k (Environment mapsRef) = helper <$> readIORef mapsRef
  where
    helper [] = Nothing
    helper (m:ms) =
      case Map.lookup k m of
        Nothing ->
          helper ms

        success ->
          success


set :: Ord k => k -> v -> Environment k v -> IO ()
set k v (Environment mapsRef) = do
  (m:ms) <- readIORef mapsRef
  writeIORef mapsRef $ Map.insert k v m : ms


extend :: Ord k => [(k, v)] -> Environment k v -> IO (Environment k v)
extend bindings (Environment mapsRef) = do
  maps <- readIORef mapsRef
  Environment <$> newIORef (Map.fromList bindings : maps)
