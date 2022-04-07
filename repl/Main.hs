module Main (main) where


import qualified Environment as Env
import qualified Interpreter as I

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)


main :: IO ()
main = displayBanner >> start


displayBanner :: IO ()
displayBanner = do
  username <- getUsername
  putStrLn $ "Hello " ++ username ++ "! This is the Monkey programming language!"
  putStrLn "Feel free to type in commands"


getUsername :: IO String
getUsername =
  fromMaybe "world" <$> lookupEnv "USER"


start :: IO ()
start = Env.empty >>= loop


loop :: I.Env -> IO ()
loop env = do
  putStr ">> "
  hFlush stdout

  line <- getLine
  (env', eitherVal) <- I.run line env
  case eitherVal of
    Right I.VNull ->
      loop env'

    Right val -> do
      print val
      loop env'

    Left err -> do
      -- TODO: Display an appropriate error message.
      print err
      loop env
